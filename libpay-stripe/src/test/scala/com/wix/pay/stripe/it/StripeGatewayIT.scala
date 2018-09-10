package com.wix.pay.stripe.it

import java.net.{SocketTimeoutException, URLDecoder}
import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpRequest, StatusCodes}
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Payment}
import com.wix.pay.stripe._
import com.wix.pay.stripe.model.Fields
import com.wix.pay.stripe.testkit.StripeITEnvironment.StripePort
import com.wix.pay.stripe.testkit.{StripeDriver, StripeError}
import com.wix.pay.testkit.LibPayTestSupport._
import com.wix.pay.{AccountException, PaymentErrorException, PaymentRejectedException}
import org.specs2.matcher.{Matcher, Matchers, ValueCheck}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

import scala.concurrent.duration._

class StripeGatewayIT extends SpecWithJUnit with Matchers {

  val driver: StripeDriver = new StripeDriver(port = StripePort)

  val authorizationParser = new JsonStripeAuthorizationParser()
  val merchantParser = new JsonStripeMerchantParser()
  val stripe = new StripeGateway(
    merchantParser = merchantParser,
    authorizationParser = authorizationParser,
    connectTimeout = Some(1.second),
    readTimeout = Some(2.seconds)
  )

  step {
    driver.start()
  }


  sequential


  trait Ctx extends Scope {
    driver.reset()

    val someMerchant = StripeMerchant("sk_live_someApiKey")
    val someMerchantKey: String = merchantParser.stringify(someMerchant)
    val someCurrencyAmount = CurrencyAmount("USD", 33.3)
    val somePayment = Payment(someCurrencyAmount, 1)
    val someCreditCard = CreditCard(
      "4012888818888",
      YearMonth(2020, 12),
      additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123"),
        holderId = Some("some holder id"),
        holderName = Some("some holder name"))))

    val someCardToken = "cardToken"
    val someChargeId = "someChargeID"
    val authorizationKey: String = authorizationParser.stringify(StripeAuthorization(someChargeId))

    def haveGatewayCode(code: Option[String]): ValueCheck[Throwable] = {
      beAnInstanceOf[PaymentRejectedException] and
        beEqualTo(code) ^^ ((e: Throwable) => {
          e match {
            case e: PaymentRejectedException => e.gatewayInternalCode
            case _ => None
          }
        })
    }

    def containsFields(pairs: (String, Matcher[String])*): Matcher[HttpRequest] = {
      havePairs(pairs:_*) ^^ getRequestFields _
    }

    private def getRequestFields(req: HttpRequest): Map[String, String] = {
      val str = new String(req.entity.asInstanceOf[HttpEntity.Strict].data.toArray)
      URLDecoder.decode(str, StandardCharsets.UTF_8.name()).split("&").map { p =>
        val pair = p.split("=")
        (pair(0), pair(1))
      }.toMap
    }

    def havePairs(pairs: (String, Matcher[String])*): Matcher[Map[String, String]] = pairs.map {
      case (k, v) ⇒ havePair(k, v)
    }.reduce(_ and _)

    def havePair(key: String, valueThat: Matcher[String]): Matcher[Map[String, String]] =
      haveKey(key) and valueThat ^^ ((_: Map[String, String])(key))
  }


  "on sale, StripeGateway" should {
    "reject on amount below minimum" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest failOnAmountBelowMinimum()

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = beAnInstanceOf[PaymentRejectedException])
    }

    "reject on this account cannot make live charges" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest failOnThisAccountCannotMakeLiveCharges()

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = beAnInstanceOf[AccountException])
    }

    "reject on your account cannot make live charges" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest failOnYourAccountCannotMakeLiveCharges()

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = beAnInstanceOf[AccountException])
    }

    "reject on expired card without code" in new Ctx {
      val emptyError = StripeError("card_error", "The card number is incorrect.")

      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest errors(StatusCodes.PaymentRequired, emptyError)

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = haveGatewayCode(None))
    }

    "reject on expired card with code" in new Ctx {
      val incorrectNumberError = StripeError("card_error", "The card number is incorrect.", "incorrect_number")

      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest errors(StatusCodes.PaymentRequired, incorrectNumberError)

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = haveGatewayCode(Some("incorrect_number")))
    }

    "reject on expired card with code and decline code" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest failOnExpiredCard()

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = haveGatewayCode(Some("card_declined|expired_card")))
    }

    "successfully execute on valid card" in new Ctx {
      driver.aCreateChargeRequest returns someChargeId
      driver.aCreateCardTokenToken returns someCardToken

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beASuccessfulTry(check = ===(someChargeId))
    }

    "successfully execute with not empty customer" in new Ctx {
      driver.aCreateChargeRequest returns someChargeId
      driver.aCreateCardTokenToken returns someCardToken

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment,
        customer = Some(someCustomer)) must beASuccessfulTry(check = ===(someChargeId))
    }

    "fail on Invalid API Key" in new Ctx {
      val stripeInvalidKeyErrorMessage: String = "Invalid API Key provided: " + someMerchantKey
      val stripeInvalidKeyError = StripeError("invalid_request_error", stripeInvalidKeyErrorMessage)

      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest errors(
        StatusCodes.Unauthorized, stripeInvalidKeyError)

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = beAnInstanceOf[PaymentErrorException])
    }

    "fail with timeout on Stripe delay" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest returns(someChargeId, Some(4.seconds))

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check =
            beAnInstanceOf[PaymentErrorException] and
            beAnInstanceOf[SocketTimeoutException] ^^ ((_: Throwable).asInstanceOf[PaymentErrorException].getCause.getCause)
          )
    }
  }


  "authorize" should {
    "gracefully fail on invalid merchant key" in new Ctx {
      driver.aCreateChargeRequest errors(
        StatusCodes.Unauthorized,
        StripeError("invalid_request_error", s"Invalid API Key provided: ${someMerchant.apiKey}"))

      stripe.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = beAnInstanceOf[PaymentErrorException])
    }

    "successfully yield an authorization key on valid request" in new Ctx {
      driver.aCreateChargeRequest returns someChargeId
      driver.aCreateCardTokenToken returns someCardToken

      stripe.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beASuccessfulTry(check = ===(authorizationKey))
    }

    "gracefully fail on rejected card" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest errors(
        StatusCodes.PaymentRequired, StripeError(
          `type` = "card_error",
          message = "Your card was declined. Your request was in test mode, but used a non test card. For a list of valid test cards, visit: https://stripe.com/docs/testing.",
          code = "card_declined"))

      stripe.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = beAnInstanceOf[PaymentRejectedException])
    }

    "gracefully fail on amount below minimum" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest failOnAmountBelowMinimum()

      stripe.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment) must beAFailedTry(check = beAnInstanceOf[PaymentRejectedException])
    }
  }


  step {
    driver.stop()
  }
}
