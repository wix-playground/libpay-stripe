package com.wix.pay.stripe

import java.math.{BigDecimal => JBigDecimal}
import java.util.Currency

object StripeAmountConversionHelper {

  def convert(amount: Double, currencyCode: String): Integer = {
    JBigDecimal.valueOf(amount).movePointRight(resolveDecimalPoints(currencyCode)).intValueExact()
  }

  private def resolveDecimalPoints(currencyCode: String): Int = {
    if (currencyCode.equalsIgnoreCase("MGA"))
      return 0
    else {
      Currency.getInstance(currencyCode.toUpperCase).getDefaultFractionDigits
    }
  }
}