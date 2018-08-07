package com.wix.pay.stripe

import com.wix.pay.model.Customer
import com.wix.pay.stripe.drivers.StripeAdditionalInfoDomain
import org.specs2.matcher.{Matcher, Matchers}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class ChargeMetadataToEmailHelperTest extends SpecWithJUnit with Matchers {

  "Stripe metadata email" should {
    "has proper metadata fields order" in new ctx {
      emailHelper.getMetadataForEmail(someCreditCard, someCustomer, someDeal) must haveOrderedKeys(keys =
        "Billing Address",
        "Customer Name",
        "Customer Phone",
        "Customer Email",
        "Customer IP",
        "Invoice Id",
        "Shipping Address",
        "OrderItemName",
        "OrderItemName2",
        "Included Charges: Tax",
        "Included Charges: Shipping"
      )
    }

    "has proper metadata fields order without empty fields" in new ctx {
      val customerWithoutIp: Option[Customer] = someCustomer.map(_.copy(ipAddress = None))

      emailHelper.getMetadataForEmail(someCreditCard, customerWithoutIp, someDeal) must haveOrderedKeys(keys =
        "Billing Address",
        "Customer Name",
        "Customer Phone",
        "Customer Email",
        "Invoice Id",
        "Shipping Address",
        "OrderItemName",
        "OrderItemName2",
        "Included Charges: Tax",
        "Included Charges: Shipping"
      )
    }

    "has required metadata values" in new ctx {
      val bAddress = someCreditCard.billingAddressDetailed.get
      val sAddress = someDeal.get.shippingAddress.get
      val charges = someDeal.get.includedCharges.get
      val customer = someCustomer.get

      emailHelper.getMetadataForEmail(someCreditCard, someCustomer, someDeal) must havePairs[String, String](
        ("Billing Address", contains(bAddress.street, bAddress.city, bAddress.postalCode, bAddress.state, bAddress.countryCode.map(_.getCountry))),
        ("Customer Name", be_==(customer.name.get.first + " " + customer.name.get.last)),
        ("Customer Phone", be_==(customer.phone.get)),
        ("Customer Email", be_==(customer.email.get)),
        ("Customer IP", be_==(customer.ipAddress.get)),
        ("Invoice Id", be_==(someDeal.get.invoiceId.get)),
        ("Shipping Address", contains(sAddress.street, sAddress.city, sAddress.postalCode, sAddress.state, sAddress.countryCode.map(_.getCountry))),
        ("OrderItemName", contains(orderItems.head.id, orderItems.head.pricePerItem.map(_.toString), orderItems.head.quantity.map(_.toString))),
        ("OrderItemName2", contains(orderItems(1).id, orderItems(1).pricePerItem.map(_.toString), orderItems(1).quantity.map(_.toString))),
        ("Included Charges: Tax", be_==(charges.tax.get.toString)),
        ("Included Charges: Shipping", be_==(charges.shipping.get.toString))
      )
    }

    "has no keys / values too long for Stripe" in new ctx {
      val tooLongCustomerName = "Freddy " * 100
      val tooLongIp = "I'm a hacker's IP, MUAHHAH! " * 100
      val customer = someCustomer.map(_.copy(
        name = someCustomer.get.name.map(_.copy(first = tooLongCustomerName)),
        ipAddress = Some(tooLongIp)
      ))

      val metadata = emailHelper.getMetadataForEmail(someCreditCard, customer, someDeal)

      metadata.keys must not(contain(stringLongerThen(40)))
      metadata.values must not(contain(stringLongerThen(500)))
    }

  }

  trait ctx extends Scope with StripeAdditionalInfoDomain {
    val emailHelper = new ChargeMetadataToEmailHelper

    def haveOrderedKeys[K](keys: K*): Matcher[Map[K, AnyRef]] =
      contain(exactly(keys: _*).inOrder) ^^ ((_: Map[K, AnyRef]).keySet)

    def havePairs[K, V](pairs: (K, Matcher[V])*): Matcher[Map[K, V]] = pairs.map {
      case (k, v) ⇒ havePair(k, v)
    }.reduce(_ and _)

    def havePair[K, V](key: K, valueThat: Matcher[V]): Matcher[Map[K, V]] =
      haveKey(key) and valueThat ^^ ((_: Map[K, V]) (key))

    def contains(patterns: Option[String]*): Matcher[String] =
      patterns.flatten.map(contain).reduce(_ and _)

    def stringLongerThen(length: Int): Matcher[String] =
      beGreaterThan(length) ^^ ((_: String).length aka "the string length")
  }

}
