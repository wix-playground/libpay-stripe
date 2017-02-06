package com.wix.pay.stripe.it

import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Payment}
import com.wix.pay.stripe._
import com.wix.pay.stripe.testkit.{StripeError, StripeITEnvironment}
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import spray.http.StatusCodes


class StripeGatewayIT extends SpecWithJUnit {

  val driver = StripeITEnvironment.stripeDriver

  val authorizationParser = new JsonStripeAuthorizationParser()
  val merchantParser = new JsonStripeMerchantParser()
  val stripe = new StripeGateway(
    merchantParser = merchantParser,
    authorizationParser = authorizationParser)


  step {
    StripeITEnvironment.setUpStripeDriver()
  }


  sequential


  trait Ctx extends Scope {
    driver.resetProbe()

    val someMerchant = new StripeMerchant("someApiKey")
    val someMerchantKey = merchantParser.stringify(someMerchant)
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
    val authorizationKey = authorizationParser.stringify(StripeAuthorization(someChargeId))
  }

  "on sale, StripeGateway" should {
    "reject on amount below minimum" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest failOnAmountBelowMinimum()

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }

    "successfully execute on valid card" in new Ctx {
      driver.aCreateChargeRequest returns someChargeId
      driver.aCreateCardTokenToken returns someCardToken

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beASuccessfulTry(
        check = ===(someChargeId)
      )
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
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }
  }

  "authorize" should {
    "gracefully fail on invalid merchant key" in new Ctx {
      driver.aCreateChargeRequest errors(
        StatusCodes.Unauthorized, StripeError("invalid_request_error", "Invalid API Key provided: " + someMerchant.apiKey))

      stripe.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }

    "successfully yield an authorization key on valid request" in new Ctx {
      driver.aCreateChargeRequest returns someChargeId
      driver.aCreateCardTokenToken returns someCardToken

      stripe.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beASuccessfulTry(
        check = ===(authorizationKey)
      )
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
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }

    "gracefully fail on amount below minimum" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest failOnAmountBelowMinimum()

      stripe.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }
  }


  step {
    StripeITEnvironment.tearDownStripeDriver()
  }
}
