package com.wix.pay.stripe.testkit


import com.stripe.Stripe
import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import spray.http._

case class StripeError(`type`: String, message: String, code: String = null)

trait StripeDriverSupport {

  def stripeProbe: EmbeddedHttpProbe

  def startStripeProbe() {
    stripeProbe.doStart()
  }

  def stopStripeProbe() {
    stripeProbe.doStop()
  }

  def resetProbe() {
    stripeProbe.handlers.clear()
  }


  abstract class Ctx(val resource: String, httpMethod: HttpMethod) {
    /** Verifies that the specified HTTP Entity matches the stubbed request. */
    def isStubbedRequestEntity(entity: HttpEntity): Boolean

    def failOnAmountBelowMinimum(): Unit = {
      val httpEntityData = "{\n  \"error\": {\n    \"type\": \"invalid_request_error\",\n    \"message\": \"Amount must be at least 30 pence\",\n    \"param\": \"amount\"\n  }\n}"
      addHandler(StatusCodes.BadRequest, httpEntityData)
    }

    def failOnExpiredCard(): Unit = {
      val httpEntityData = "{\n  \"error\": {\n    \"type\": \"card_error\",\n    \"message\": \"The card has expired.\",\n    \"code\": \"card_declined\",\n    \"decline_code\": \"expired_card\"\n  }\n}"
      addHandler(StatusCodes.PaymentRequired, httpEntityData)
    }

    def errors(statusCode: StatusCode, error: StripeError) {
      val code = Option(error.code).map(c => ",\n    \"code\": \"" + c + "\"")
      val httpEntityData = "{\n  \"error\": {\n    \"type\": \"" + error.`type` + "\",\n    \"message\": \"" + error.message + "\"" + code.getOrElse("") + "\n  }\n}"
      addHandler(statusCode, httpEntityData)
    }

    private def addHandler(statusCode: StatusCode, httpEntityData: String): Any = {
      stripeProbe.handlers += {
        case HttpRequest(
        httpMethod,
        Uri.Path(`resource`),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) =>
          HttpResponse(
            status = statusCode,
            entity = HttpEntity(ContentTypes.`application/json`,
              httpEntityData))
      }
    }
  }
}

class StripeDriver(port: Int) extends StripeDriverSupport with ChargeStripeDriver with AccountStripeDriver with CreateCardTokenDriver {
  val stripeProbe = new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler)
}

trait CreateCardTokenDriver extends StripeDriverSupport {
  def aCreateCardTokenToken(): CreateCardTokenCtx = {
    new CreateCardTokenCtx()
  }

  class CreateCardTokenCtx() extends Ctx("/v1/tokens", HttpMethods.POST) {

    def returns(token: String) {
      stripeProbe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`resource`),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(
              ContentTypes.`application/json`,
              s"""{"id": "$token","livemode": false,"created": 1443730865,"used": false,"object": "token","type": "card","card": {"id": "card_16rDY52o5xfNtGQB2aH137qj", "object": "card","last4": "4242","brand": "Visa","funding": "credit","exp_month": 12,"exp_year": 2016,"fingerprint": "lrdUhAbthd7RAFVh","country": "US"},"client_ip": "127.0.0.1"}"""))
      }
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify accessToken, currencyAmount. creditCard
      true
    }
  }
}

trait ChargeStripeDriver extends StripeDriverSupport {
  def aCreateChargeRequest(): CreateChargeCtx = {
    new CreateChargeCtx()
  }

  class CreateChargeCtx() extends Ctx("/v1/charges", HttpMethods.POST) {

    def returns(chargeId: String) {
      stripeProbe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`resource`),
        _,
        entity,
        _) if entityWithSource(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(
              ContentTypes.`application/json`,
              "{\n  \"id\": \"" + chargeId + "\",\n  \"object\": \"charge\",\n  \"created\": 1422879936,\n  \"livemode\": false,\n  \"paid\": true,\n  \"amount\": 2488,\n  \"currency\": \"usd\",\n  \"refunded\": false,\n  \"captured\": true,\n  \"card\": {\n    \"id\": \"card_15RjGl2eZvKYlo2CcBf3W6AZ\",\n    \"object\": \"card\",\n    \"last4\": \"4242\",\n    \"brand\": \"Visa\",\n    \"funding\": \"credit\",\n    \"exp_month\": 3,\n    \"exp_year\": 2015,\n    \"fingerprint\": \"Xt5EWLLDS7FJjR1c\",\n    \"country\": \"US\",\n    \"name\": \"Christ\",\n    \"address_line1\": null,\n    \"address_line2\": null,\n    \"address_city\": null,\n    \"address_state\": null,\n    \"address_zip\": null,\n    \"address_country\": null,\n    \"cvc_check\": \"pass\",\n    \"address_line1_check\": null,\n    \"address_zip_check\": null,\n    \"dynamic_last4\": null,\n    \"customer\": null\n  },\n  \"balance_transaction\": \"txn_15PZty2eZvKYlo2CmvbHgq0x\",\n  \"failure_message\": null,\n  \"failure_code\": null,\n  \"amount_refunded\": 0,\n  \"customer\": null,\n  \"invoice\": null,\n  \"description\": \"Checkout description\",\n  \"dispute\": null,\n  \"metadata\": {\n  },\n  \"statement_descriptor\": null,\n  \"fraud_details\": {\n  },\n  \"receipt_email\": null,\n  \"receipt_number\": null,\n  \"shipping\": null,\n  \"refunds\": {\n    \"object\": \"list\",\n    \"total_count\": 0,\n    \"has_more\": false,\n    \"url\": \"/v1/charges/" + chargeId + "/refunds\",\n    \"data\": [\n\n    ]\n  }\n}"))
        }
    }

    private def entityWithSource(entity: HttpEntity): Boolean = {
      entity.asString.contains("source")
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify accessToken, currencyAmount. creditCard
      true
    }
  }
}

trait AccountStripeDriver extends StripeDriverSupport {
  def aRetrieveAccountFor(): RetrieveAccountCtx = {
    new RetrieveAccountCtx()
  }

  class RetrieveAccountCtx() extends Ctx("/v1/account", HttpMethods.GET) {

    def succeeds() {
      stripeProbe.handlers += {
        case HttpRequest(
        HttpMethods.GET,
        Uri.Path(`resource`),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(
              ContentTypes.`application/json`,
            """{
              |  "id": "acct_16aSJ6EMKwVXA0As",
              |  "email": "natans@wix.com",
              |  "statement_descriptor": null,
              |  "display_name": null,
              |  "timezone": "Asia/Jerusalem",
              |  "details_submitted": false,
              |  "charges_enabled": false,
              |  "transfers_enabled": false,
              |  "currencies_supported": [
              |    "usd",
              |    "aed",
              |    "afn",
              |    "all",
              |    "amd",
              |    "ang",
              |    "aoa",
              |    "ars",
              |    "aud",
              |    "awg",
              |    "azn",
              |    "bam",
              |    "bbd",
              |    "bdt",
              |    "bgn",
              |    "bif",
              |    "bmd",
              |    "bnd",
              |    "bob",
              |    "brl",
              |    "bsd",
              |    "bwp",
              |    "bzd",
              |    "cad",
              |    "cdf",
              |    "chf",
              |    "clp",
              |    "cny",
              |    "cop",
              |    "crc",
              |    "cve",
              |    "czk",
              |    "djf",
              |    "dkk",
              |    "dop",
              |    "dzd",
              |    "egp",
              |    "etb",
              |    "eur",
              |    "fjd",
              |    "fkp",
              |    "gbp",
              |    "gel",
              |    "gip",
              |    "gmd",
              |    "gnf",
              |    "gtq",
              |    "gyd",
              |    "hkd",
              |    "hnl",
              |    "hrk",
              |    "htg",
              |    "huf",
              |    "idr",
              |    "ils",
              |    "inr",
              |    "isk",
              |    "jmd",
              |    "jpy",
              |    "kes",
              |    "kgs",
              |    "khr",
              |    "kmf",
              |    "krw",
              |    "kyd",
              |    "kzt",
              |    "lak",
              |    "lbp",
              |    "lkr",
              |    "lrd",
              |    "lsl",
              |    "ltl",
              |    "mad",
              |    "mdl",
              |    "mga",
              |    "mkd",
              |    "mnt",
              |    "mop",
              |    "mro",
              |    "mur",
              |    "mvr",
              |    "mwk",
              |    "mxn",
              |    "myr",
              |    "mzn",
              |    "nad",
              |    "ngn",
              |    "nio",
              |    "nok",
              |    "npr",
              |    "nzd",
              |    "pab",
              |    "pen",
              |    "pgk",
              |    "php",
              |    "pkr",
              |    "pln",
              |    "pyg",
              |    "qar",
              |    "ron",
              |    "rsd",
              |    "rub",
              |    "rwf",
              |    "sar",
              |    "sbd",
              |    "scr",
              |    "sek",
              |    "sgd",
              |    "shp",
              |    "sll",
              |    "sos",
              |    "srd",
              |    "std",
              |    "svc",
              |    "szl",
              |    "thb",
              |    "tjs",
              |    "top",
              |    "try",
              |    "ttd",
              |    "twd",
              |    "tzs",
              |    "uah",
              |    "ugx",
              |    "uyu",
              |    "uzs",
              |    "vnd",
              |    "vuv",
              |    "wst",
              |    "xaf",
              |    "xcd",
              |    "xof",
              |    "xpf",
              |    "yer",
              |    "zar",
              |    "zmw"
              |  ],
              |  "default_currency": "usd",
              |  "country": "US",
              |  "object": "account",
              |  "business_name": null,
              |  "business_url": null,
              |  "support_phone": null,
              |  "business_logo": null,
              |  "managed": false
              |}
              |""".stripMargin))
      }
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify accessToken, currencyAmount. creditCard
      true
    }
  }
}

class HappyPathStripeDriver(port: Int) extends StripeDriverSupport
  with ChargeStripeDriver with AccountStripeDriver with CreateCardTokenDriver {
  // Ugly, but seems to be the only way to do this
  Stripe.overrideApiBase(s"http://localhost:$port")

  val stripeProbe = new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler)
  aCreateChargeRequest.returns(chargeId = "someChargeId")
  aRetrieveAccountFor.succeeds()
  aCreateCardTokenToken.returns(token = "someToken")
}