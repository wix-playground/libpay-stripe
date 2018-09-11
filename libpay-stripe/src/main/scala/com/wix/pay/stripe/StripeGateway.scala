package com.wix.pay.stripe


import com.stripe.exception.{CardException, InvalidRequestException, StripeException}
import com.stripe.model.{Charge, Token}
import com.stripe.net.RequestOptions
import com.stripe.net.RequestOptions.RequestOptionsBuilder
import com.wix.pay._
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.feature.FeatureRegistry
import com.wix.pay.model._
import com.wix.pay.stripe.model.Fields

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

class StripeGateway(merchantParser: StripeMerchantParser = new JsonStripeMerchantParser,
                    authorizationParser: StripeAuthorizationParser = new JsonStripeAuthorizationParser,
                    additionalInfoMapper: StripeAdditionalInfoMapper = new StripeAdditionalInfoMapper,
                    metadataHelper: ChargeMetadataHelper = new ChargeMetadataHelper,
                    useAlternativeMetadata: Boolean = false,
                    sendReceipts: Boolean = false,
                    connectTimeout: Option[Duration] = None,
                    readTimeout: Option[Duration] = None) extends PaymentGateway {

  private def createCharge(apiKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal], autoCapture: Boolean): Charge = {
    val token = retrieveCardToken(apiKey, creditCard)

    val metadata = if (useAlternativeMetadata)
      metadataHelper.getMetadata(creditCard, customer, deal).asJava
    else
      additionalInfoMapper.createMap(creditCard, customer, deal)

    val baseParams = Map(
      Fields.amount -> StripeAmountConversionHelper.convert(currencyAmount.amount, currencyAmount.currency),
      Fields.currency -> currencyAmount.currency,
      Fields.source -> token.getId,
      Fields.capture -> autoCapture.asInstanceOf[java.lang.Boolean],
      Fields.metadata -> metadata
    )

    val receiptParams = for {
      c ← customer
      email ← c.email
      if sendReceipts
    } yield Fields.receiptEmail → email

    val fraudParams = if (FeatureRegistry.isEnabled("StripeSendFraudDetectionInfo")) {
      Map(
        Fields.ip -> customer.flatMap(_.ipAddress).orNull,
        Fields.userAgent -> customer.flatMap(_.userAgent).orNull,
        Fields.referrer -> customer.flatMap(_.referrer).orNull,
        Fields.deviceId -> customer.flatMap(_.deviceId).orNull,
        Fields.externalId -> customer.flatMap(_.id).orNull
      ).filter(t => t._2 != null && !t._2.isEmpty)
    } else Nil

    val params = baseParams ++ receiptParams ++ fraudParams
    Charge.create(params, requestOptionsFor(apiKey))
  }

  private def retrieveCardToken(apiKey: String, creditCard: CreditCard): Token = {
    // credit card is tokenized before being assigned to 'Charge' in order to bypass some restrictions
    // Stripe puts on using credit card details when using apiKey retrieved using 'Stripe Connect' (OAuth)
    // Stripe prefers developers use Stripe.Js which is guaranteed to be PCI compliant...
    // see 'https://stripe.com/docs/connect/payments-fees' Stripe.Js box
    val params = Map(
      Fields.card -> CreditCardMapper.cardToParams(creditCard)
    )
    Token.create(params, requestOptionsFor(apiKey))
  }

  private def requestOptionsFor(apiKey: String): RequestOptions = RequestOptions.builder
    .setApiKey(apiKey)
    .setWith(_.setConnectTimeout, connectTimeout.map(_.toMillis.intValue()))
    .setWith(_.setReadTimeout, readTimeout.map(_.toMillis.intValue()))
    .build

  private implicit class `RequestOptionsTimeoutBuilder`(builder: RequestOptionsBuilder) {
    def setWith[A](builderModifier: RequestOptionsBuilder ⇒ A ⇒ RequestOptionsBuilder, maybeParam: Option[A]) = maybeParam match {
      case Some(param) ⇒ builderModifier(builder)(param)
      case None ⇒ builder
    }
  }

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      require(payment.installments == 1, "Stripe does not support installments")

      val merchant = parseAndValidateMerchantKey(merchantKey)

      val charge = createCharge(
        apiKey = merchant.apiKey,
        creditCard = creditCard,
        currencyAmount = payment.currencyAmount,
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
      val merchant = parseAndValidateMerchantKey(merchantKey)
      val authorization = authorizationParser.parse(authorizationKey)

      val charge = Charge.retrieve(authorization.chargeId, requestOptionsFor(merchant.apiKey))

      val params = Map(
        Fields.amount -> StripeAmountConversionHelper.convert(amount, charge.getCurrency)
      )
      val captured = charge.capture(params, requestOptionsFor(merchant.apiKey))
      captured.getId
    } match {
      case Success(chargeId) => Success(chargeId)
      case Failure(e: StripeException) => Failure(translateStripeException(e))
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      require(payment.installments == 1, "Stripe does not support installments")

      val merchant = parseAndValidateMerchantKey(merchantKey)

      val charge = createCharge(
        apiKey = merchant.apiKey,
        creditCard = creditCard,
        currencyAmount = payment.currencyAmount,
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
      val merchant = parseAndValidateMerchantKey(merchantKey)
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
      case e: CardException =>
        new PaymentRejectedException(e.getMessage, e, extractCardExceptionCode(e))
      case AmountBelowMinimum(amountBelowMinimumException) =>
        new PaymentRejectedException(amountBelowMinimumException.getMessage, amountBelowMinimumException)
      case AccountCannotMakeLiveCharges(accountCannotMakeLiveChargesException) =>
        new AccountException(accountCannotMakeLiveChargesException.getMessage, accountCannotMakeLiveChargesException)
      case _ => new PaymentErrorException(e.getMessage, e)
    }
  }

  private def extractCardExceptionCode(e: CardException) = {
    val errorCode = Option(e.getCode)
    val errorDeclineCode = Option(e.getDeclineCode)

    (errorCode, errorDeclineCode) match {
      case (None, _) => None
      case (_, None) => errorCode
      case (Some(code), Some(declineCode)) => Some(s"$code|$declineCode")
    }
  }

  private def parseAndValidateMerchantKey(merchantKey: String): StripeMerchant = {
    val merchant = merchantParser.parse(merchantKey)

    // Stripe uses secret keys (sk_XXX) and publishable keys (pk_XXX), we need the first.
    // Unfortunately, when passed a publishable key, Stripe may fail with a non-indicative error message.
    //
    // For example, when trying to charge the test card "4222222222222", Stripe fails with
    // "Your card was declined. Your request was in live mode, but used a known test card."
    // (which is treated as a rejected payment),
    // and not with
    // "This API call cannot be made with a publishable API key. Please use a secret API key."
    // (which is treated as an error).
    //
    // To work around this, we explicitly validate the API key looks like a secret key.
    require(merchant.apiKey.startsWith("sk_"), "Invalid Stripe secret key: doesn't start with sk_")

    merchant
  }
}

/** @see <a href="https://support.stripe.com/questions/what-is-the-minimum-amount-i-can-charge-with-stripe">What is the minimum amount I can charge with Stripe?</a> */
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

private object AccountCannotMakeLiveCharges {
  def unapply(e: InvalidRequestException): Option[InvalidRequestException] = {
    if (e.getMessage.contains("account cannot currently make live charges")) {
      Some(e)
    } else {
      None
    }
  }
}
