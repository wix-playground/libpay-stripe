package com.wix.pay.stripe

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonStripeAuthorizationParser() extends StripeAuthorizationParser {
  implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): StripeAuthorization = {
    Serialization.read[StripeAuthorization](authorizationKey)
  }

  override def stringify(authorization: StripeAuthorization): String = {
    Serialization.write(authorization)
  }
}
