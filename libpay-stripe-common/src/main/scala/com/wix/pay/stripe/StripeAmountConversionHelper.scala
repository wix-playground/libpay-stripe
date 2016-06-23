package com.wix.pay.stripe

import java.math.{BigDecimal => JBigDecimal}

object StripeAmountConversionHelper {
  private val defaultDecimalPoints: Int = 2

  private val currencyPoints = Set(
    "BIF", "CLP", "DJF",
    "GNF", "JPY", "KMF",
    "KRW", "MGA", "PYG",
    "RWF", "VND", "VUV",
    "XAF", "XOF", "XPF")

  def convert(amount: Double, currency: String): Integer = {
    JBigDecimal.valueOf(amount).movePointRight(resolveDecimalPoints(currency)).intValueExact()
  }

  private def resolveDecimalPoints(currency: String): Int = {
    val decimalPoints = findCurrency(currency).map(_ => 0)
    decimalPoints.getOrElse(defaultDecimalPoints)
  }

  private def findCurrency(currency: String) = {
    currencyPoints.find(_.equalsIgnoreCase(currency))
  }
}