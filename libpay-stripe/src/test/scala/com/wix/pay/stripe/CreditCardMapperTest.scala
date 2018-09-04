package com.wix.pay.stripe

import java.util

import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import org.specs2.matcher.Matcher
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class CreditCardMapperTest extends SpecificationWithJUnit {
  "CreditCardMapper" should {
    "create params with all relevant fields " in new ctx {
      CreditCardMapper.cardToParams(someCreditCard) must
        havePair("address_line1", billingAddress.get) and
        havePair("address_zip", billingPostalCode.get) and
        havePair("name", holderName.get)
    }

    "return params without address and zip code as they are empty Strings " in new ctx {
      CreditCardMapper.cardToParams(emptyFieldsCreditCard) must
        not(haveKey("address_line1")) and
        not(haveKey("address_zip"))
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

    def havePair(key: String, value: AnyRef): Matcher[util.LinkedHashMap[String, Object]] =
      be_==(value) ^^ ((_: util.LinkedHashMap[String, Object]).get(key))

    def haveKey(key: String): Matcher[util.LinkedHashMap[String, Object]] =
      beTrue ^^ ((_: util.LinkedHashMap[String, Object]).containsKey(key))
  }

}
