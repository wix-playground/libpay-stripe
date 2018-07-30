package com.wix.pay.stripe

import com.wix.pay.stripe.drivers.{StripeAdditionalInfoDomain, StripeMatchers}
import org.specs2.matcher.Matchers
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope


class StripeAdditionalInfoMapperTest extends SpecWithJUnit with Matchers with StripeMatchers {

  trait Ctx extends Scope with StripeAdditionalInfoDomain {
    val mapper = new StripeAdditionalInfoMapper()

  }

  "Stripe additional info mapper" should {
    "map additional Info into a map" in new Ctx {
      val map: MappedParams = mapper.createMap(someCreditCard, someCustomer, someDeal)
      map must {
        containBillingAddress(billingAddress.get) and
          containCustomer(someCustomer.get) and
          containInvoiceId(someInvoiceId.get) and
          containShippingAddress(someShippingAddress.get) and
          containOrderItems(orderItems) and
          containIncludedCharges(includedCharges.get)
      }

    }
  }
}

