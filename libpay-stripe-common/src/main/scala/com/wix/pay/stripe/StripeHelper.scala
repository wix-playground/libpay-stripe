package com.wix.pay.stripe

import java.math.{BigDecimal => JBigDecimal}

object StripeHelper {
  def toStripeAmount(amount: Double): Integer = {
    JBigDecimal.valueOf(amount).movePointRight(2).intValueExact()
  }
}
