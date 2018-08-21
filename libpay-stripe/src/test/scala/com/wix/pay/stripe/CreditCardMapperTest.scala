package com.wix.pay.stripe

import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, PublicCreditCardOptionalFields, YearMonth}
import com.wix.pay.stripe.drivers.StripeMatchers
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class CreditCardMapperTest extends SpecificationWithJUnit with StripeMatchers {
  "CreditCardMapper" should {
    "create params with all relevant fields " in new ctx{
      CreditCardMapper.cardToParams(someCreditCard) must haveFieldParams(expectedFields)
    }

    "return params without address and zip code as they are empty Strings " in new ctx {
      CreditCardMapper.cardToParams(emptyFieldsCreditCard) must not(haveAnyEmptyFields)
    }
  }

  trait ctx extends Scope {
    val holderId: Some[String] = Some("some holder id")
    val holderName: Some[String] = Some("some holder name")
    val billingAddress: Some[String] = Some("some address")
    val billingPostalCode: Some[String] = Some("some postal Code")

    val someCreditCard = CreditCard(
      "4012888818888",
      YearMonth(2020, 12),
      additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123"),
        holderId = holderId,
        holderName = holderName,
        billingAddress = billingAddress,
        billingPostalCode = billingPostalCode)))

    val emptyFieldsCreditCard = CreditCard(
      "4012888818888",
      YearMonth(2020, 12),
      additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123"),
        holderId = holderId,
        holderName = holderName,
        billingAddress = Some(""),
        billingPostalCode = Some(""))))

    val expectedFields = PublicCreditCardOptionalFields(holderId, holderName, billingAddress, billingPostalCode)
  }
}
