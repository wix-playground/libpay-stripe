package com.wix.pay.stripe

import org.specs2.mutable.SpecificationWithJUnit

class StripeAmountConversionHelperTest extends SpecificationWithJUnit {
  val currencyDoubleAmountToIntAmount = Map[(Double,String),Int](
    (17.0, "USD") -> 1700,
    (17.0, "UsD") -> 1700,
    (17.0, "BIF") -> 17,
    (17.0, "bif") -> 17,
    (17.0, "CLP") -> 17,
    (17.0, "DJF") -> 17,
    (17.0, "GNF") -> 17,
    (17.0, "JPY") -> 17,
    (17.0, "KMF") -> 17,
    (17.0, "KRW") -> 17,
    (17.0, "MGA") -> 17,
    (17.0, "PYG") -> 17,
    (17.0, "RWF") -> 17,
    (17.0, "VND") -> 17,
    (17.0, "VUV") -> 17,
    (17.0, "XAF") -> 17,
    (17.0, "XOF") -> 17,
    (17.0, "XPF") -> 17
  )

  currencyDoubleAmountToIntAmount foreach { case (currencyDoubleAmount, intAmount) =>
    s"$currencyDoubleAmount should convert to $intAmount" >> {
      val (doubleAmount, currency) = currencyDoubleAmount
      StripeAmountConversionHelper.convert(doubleAmount, currency) mustEqual intAmount
    }
  }
}