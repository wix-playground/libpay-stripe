package com.wix.pay.stripe.it

import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.CurrencyAmount
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
    val someCreditCard = CreditCard(
      "4012888818888",
      YearMonth(2020, 12),
      additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123"),
        holderId = Some("some holder id"),
        holderName = Some("some holder name"))))

    val someCardToken = "cardToken"
  }

  "sale" should {
    "gracefully fail on amount below minimum" in new Ctx {
      driver.aCreateCardTokenToken returns someCardToken
      driver.aCreateChargeRequest failOnAmountBelowMinimum()

      stripe.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
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
        currencyAmount = someCurrencyAmount
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }

    "successfully yield an authorization key on valid request" in new Ctx {
      val someChargeId = "someChargeID"
      val authorizationKey = authorizationParser.stringify(StripeAuthorization(someChargeId))

      driver.aCreateChargeRequest returns someChargeId
      driver.aCreateCardTokenToken returns someCardToken

      stripe.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount
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
        currencyAmount = someCurrencyAmount
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
        currencyAmount = someCurrencyAmount
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }
  }


  step {
    StripeITEnvironment.tearDownStripeDriver()
  }
}
