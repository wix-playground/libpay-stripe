package com.wix.pay.stripe


import com.stripe.exception.{CardException, InvalidRequestException, StripeException}
import com.stripe.model.{Charge, Token}
import com.stripe.net.RequestOptions
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.stripe.model.Fields
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway, PaymentRejectedException}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

class StripeGateway(merchantParser: StripeMerchantParser = new JsonStripeMerchantParser,
                    authorizationParser: StripeAuthorizationParser = new JsonStripeAuthorizationParser,
                    additionalInfoMapper: StripeAdditionalInfoMapper = new StripeAdditionalInfoMapper) extends PaymentGateway {
  val creditCardMapper = new CreditCardMapper

  private def createCharge(apiKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal], autoCapture: Boolean): Charge = {
    val token = retrieveCardToken(apiKey, creditCard)

    val params = Map(
      Fields.amount -> StripeHelper.toStripeAmount(currencyAmount.amount),
      Fields.currency -> currencyAmount.currency,
      Fields.source -> token.getId,
      Fields.capture -> autoCapture.asInstanceOf[java.lang.Boolean],
      Fields.metadata -> additionalInfoMapper.createMap(creditCard, customer, deal)
    )
    Charge.create(params, requestOptionsFor(apiKey))
  }

  private def retrieveCardToken(apiKey: String, creditCard: CreditCard): Token = {
    // credit card is tokenized before being assigned to 'Charge' in order to bypass some restrictions
    // Stripe puts on using credit card details when using apiKey retrieved using 'Stripe Connect' (OAuth)
    // Stripe prefers developers use Stripe.Js which is guaranteed to be PCI compliant...
    // see 'https://stripe.com/docs/connect/payments-fees' Stripe.Js box
    val params = Map(
      Fields.card -> creditCardMapper.cardToParams(creditCard)
    )
    Token.create(params, requestOptionsFor(apiKey))
  }

  private def requestOptionsFor(apiKey: String): RequestOptions = {
    RequestOptions.builder.setApiKey(apiKey).build
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)

      val charge = createCharge(
        apiKey = merchant.apiKey,
        creditCard = creditCard,
        currencyAmount = currencyAmount,
        customer = customer,
        deal = deal,
        autoCapture = false)

      authorizationParser.stringify(StripeAuthorization(charge.getId))
    } match {
      case Success(authorizationKey) => Success(authorizationKey)
      case Failure(e: StripeException) =>  Failure(translateStripeException(e))
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val authorization = authorizationParser.parse(authorizationKey)

      val charge = Charge.retrieve(authorization.chargeId, requestOptionsFor(merchant.apiKey))

      val params = Map(
        Fields.amount -> StripeHelper.toStripeAmount(amount)
      )
      val captured = charge.capture(params, requestOptionsFor(merchant.apiKey))
      captured.getId
    } match {
      case Success(chargeId) => Success(chargeId)
      case Failure(e: StripeException) => Failure(translateStripeException(e))
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def sale(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)

      val charge = createCharge(
        apiKey = merchant.apiKey,
        creditCard = creditCard,
        currencyAmount = currencyAmount,
        customer = customer,
        deal = deal,
        autoCapture = true)

      charge.getId
    } match {
      case Success(chargeId) => Success(chargeId)
      case Failure(e: StripeException) => Failure(translateStripeException(e))
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val authorization = authorizationParser.parse(authorizationKey)

      // Stripe doesn't support voiding an authorization - you can either issue a full refund or just wait 7 days.
      // According to Stripe support, the two are equivalent (after 7 days) from both the payer's and payee's perspectives.
      // So for now, we just wait it out.
      val charge = Charge.retrieve(authorization.chargeId, requestOptionsFor(merchant.apiKey))
      charge.getId
    }
  }

  private def translateStripeException(e: StripeException): PaymentException = {
    e match {
      case e: CardException => new PaymentRejectedException(e.getMessage, e)
      case AmountBelowMinimum(amountBelowMinimumException) =>
        new PaymentRejectedException(amountBelowMinimumException.getMessage, amountBelowMinimumException)
      case _ => new PaymentErrorException(e.getMessage, e)
    }
  }
}

private object AmountBelowMinimum {
  def unapply(e: InvalidRequestException): Option[InvalidRequestException] = {
    // Reliance on the error message is crude, but seems necessary.
    // The full error message goes something like "Amount must be at least 30 pence"
    if ((e.getParam == Fields.amount) && e.getMessage.startsWith("Amount must be at least ")) {
      Some(e)
    } else {
      None
    }
  }
}
