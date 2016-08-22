package com.wix.pay.stripe

import java.util

import com.wix.pay.creditcard.CreditCard

object CreditCardMapper {
  def cardToParams(creditCard: CreditCard): util.LinkedHashMap[String, Object] = {
    val params = new util.LinkedHashMap[String, Object]()

    params.put("number", creditCard.number)

    params.put("exp_year", creditCard.expiration.year.asInstanceOf[Integer])
    params.put("exp_month", creditCard.expiration.month.asInstanceOf[Integer])

    creditCard.holderName foreach { holderName => params.put("name", holderName) }
    creditCard.billingAddress foreach { billingAddress => putNonEmptyField("address_line1", params, billingAddress) }
    creditCard.billingPostalCode foreach { billingPostalCode => putNonEmptyField("address_zip", params, billingPostalCode)  }
    creditCard.csc foreach { csc => params.put("cvc", csc) }

    params
  }

  private def putNonEmptyField(keyName: String, params: util.LinkedHashMap[String, Object], fieldValue: String) = {
    if (fieldValue != "")
      params.put(keyName, fieldValue)
  }
}
