package com.wix.pay.stripe.testkit


import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import com.stripe.Stripe
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory._


class StripeDriver(server: StubWebServer) {

  def this(port: Int) {
    this(aStubWebServer.onPort(port).build)
    // Ugly, but seems to be the only way to do this
    Stripe.overrideApiBase(s"http://localhost:$port")
  }

  def start(): Unit = server.start()
  def stop(): Unit = server.stop()
  def reset(): Unit = server.replaceWith()


  def aCreateCardTokenToken: CreateCardTokenCtx = new CreateCardTokenCtx()
  def aRetrieveAccountFor: RetrieveAccountCtx = new RetrieveAccountCtx()
  def aCreateChargeRequest: CreateChargeCtx = new CreateChargeCtx()


  abstract class Ctx(val resource: String) {
    def failOnAmountBelowMinimum(): Unit = {
      val httpEntityData =
        """{
          |  "error": {
          |    "type": "invalid_request_error",
          |    "message": "Amount must be at least 30 pence",
          |    "param": "amount"
          |  }
          |}""".stripMargin

      addHandler(StatusCodes.BadRequest, httpEntityData)
    }

    def failOnExpiredCard(): Unit = {
      val httpEntityData =
        """{
          |  "error": {
          |    "type": "card_error",
          |    "message": "The card has expired.",
          |    "code": "card_declined",
          |    "decline_code": "expired_card"
          |  }
          |}""".stripMargin

      addHandler(StatusCodes.PaymentRequired, httpEntityData)
    }

    def errors(statusCode: StatusCode, error: StripeError) {
      val code = Option(error.code).map(c => s""", "code": "$c"""")
      val httpEntityData =
        s"""{
           |  "error": {
           |    "type": "${error.`type`}",
           |    "message": "${error.message}"${code.getOrElse("")}
           |  }
           |}""".stripMargin
      addHandler(statusCode, httpEntityData)
    }


    def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify accessToken, currencyAmount. creditCard
      true
    }

    protected def addHandler(statusCode: StatusCode, httpEntityData: String): Any = {
      server.appendAll {
        case HttpRequest(
          _,
          Path(`resource`),
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


  class RetrieveAccountCtx() extends Ctx("/v1/account") {
    def succeeds() {
      addHandler(
        StatusCodes.OK,
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
          |}""".stripMargin)
    }
  }


  class CreateCardTokenCtx() extends Ctx("/v1/tokens") {
    def returns(token: String) {
      addHandler(
        StatusCodes.OK,
        s"""{
           |  "id": "$token",
           |  "livemode": false,
           |  "created": 1443730865,
           |  "used": false,
           |  "object": "token",
           |  "type": "card",
           |  "card": {
           |    "id": "card_16rDY52o5xfNtGQB2aH137qj",
           |    "object": "card",
           |    "last4": "4242",
           |    "brand": "Visa",
           |    "funding": "credit",
           |    "exp_month": 12,
           |    "exp_year": 2016,
           |    "fingerprint": "lrdUhAbthd7RAFVh",
           |    "country": "US"
           |  },
           |  "client_ip": "127.0.0.1"}""".stripMargin)
    }
  }


  class CreateChargeCtx() extends Ctx("/v1/charges") {
    def returns(chargeId: String) {
      addHandler(
        StatusCodes.OK,
        s"""{
           |  "id": "$chargeId",
           |  "object": "charge",
           |  "created": 1422879936,
           |  "livemode": false,
           |  "paid": true,
           |  "amount": 2488,
           |  "currency": "usd",
           |  "refunded": false,
           |  "captured": true,
           |  "card": {
           |    "id": "card_15RjGl2eZvKYlo2CcBf3W6AZ",
           |    "object": "card",
           |    "last4": "4242",
           |    "brand": "Visa",
           |    funding": "credit",
           |    "exp_month": 3,
           |    "exp_year": 2015,
           |    "fingerprint": "Xt5EWLLDS7FJjR1c",
           |    "country": "US",
           |    "name": "Christ",
           |    "address_line1": null,
           |    "address_line2": null,
           |    "address_city": null,
           |    "address_state": null,
           |    "address_zip": null,
           |    "address_country": null,
           |    "cvc_check": "pass",
           |    "address_line1_check": null,
           |    "address_zip_check": null,
           |    "dynamic_last4": null,
           |    "customer": null
           |  },
           |  "balance_transaction": "txn_15PZty2eZvKYlo2CmvbHgq0x",
           |  "failure_message": null,
           |  "failure_code": null,
           |  "amount_refunded": 0,
           |  "customer": null,
           |  "invoice": null,
           |  "description": "Checkout description",
           |  "dispute": null,
           |  "metadata": {},
           |  "statement_descriptor": null,
           |  "fraud_details": {},
           |  "receipt_email": null,
           |  "receipt_number": null,
           |  "shipping": null,
           |  "refunds": {
           |    "object": "list",
           |    "total_count": 0,
           |    "has_more": false,
           |    "url": "/v1/charges/$chargeId/refunds\",
           |    "data": []
           |  }
           |}""".stripMargin)
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      entity.extractAsString.contains("source")
    }
  }
}


case class StripeError(`type`: String, message: String, code: String = null)
