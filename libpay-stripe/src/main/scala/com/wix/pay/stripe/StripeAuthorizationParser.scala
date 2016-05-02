package com.wix.pay.stripe

trait StripeAuthorizationParser {
  def parse(authorizationKey: String): StripeAuthorization
  def stringify(authorization: StripeAuthorization): String
}
