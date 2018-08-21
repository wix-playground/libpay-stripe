package com.wix.pay.stripe.drivers

import java.util

import com.wix.pay.creditcard.{AddressDetailed, PublicCreditCardOptionalFields}
import com.wix.pay.model.{Customer, IncludedCharges, OrderItem, ShippingAddress}
import com.wix.pay.stripe._
import org.specs2.matcher.{Matcher, Matchers}

trait StripeMatchers { self : Matchers =>

  def containBillingAddress(billingAddress: AddressDetailed): Matcher[MappedParams] = {
    be_===(billingAddress) ^^ {
      StripeAdditionalInfoReConstructor.reconstructBillingAddress(_: MappedParams)
    }
  }

  def containCustomer(customer: Customer): Matcher[MappedParams] = {
    be_===(customer) ^^ {
      StripeAdditionalInfoReConstructor.reconstructCustomer(_: MappedParams)
    }
  }

  def containInvoiceId(invoiceId: String): Matcher[MappedParams] = {
    be_===(invoiceId) ^^ {
      StripeAdditionalInfoReConstructor.reconstructInvoiceId(_: MappedParams)
    }
  }

  def containShippingAddress(shippingAddress: ShippingAddress): Matcher[MappedParams] = {
    be_===(shippingAddress) ^^ {
      StripeAdditionalInfoReConstructor.reconstructShippingAddress(_: MappedParams)
    }
  }

  def containOrderItems(orderItems: Seq[OrderItem]): Matcher[MappedParams] = {
    be_===(orderItems) ^^ {
      StripeAdditionalInfoReConstructor.reconstructOrderItems(_: MappedParams)
    }
  }

  def containIncludedCharges(includedCharges: IncludedCharges): Matcher[MappedParams] = {
    be_===(includedCharges) ^^ {
      StripeAdditionalInfoReConstructor.reconstructIncludedCharges(_: MappedParams)
    }
  }

  def haveFieldParams(fields: PublicCreditCardOptionalFields): Matcher[util.LinkedHashMap[String, Object]] = {
    be_===(fields.billingAddress.get) ^^ {(_:util.LinkedHashMap[String, Object]).get("address_line1").toString} and
      be_===(fields.billingPostalCode.get) ^^ {(_:util.LinkedHashMap[String, Object]).get("address_zip").toString} and
      be_===(fields.holderName.get) ^^ {(_:util.LinkedHashMap[String, Object]).get("name").toString}
  }

  def haveAnyEmptyFields(): Matcher[util.LinkedHashMap[String, Object]] = {
    beTrue ^^ {(_:util.LinkedHashMap[String, Object]).containsKey("address_line1")} or
      beTrue ^^ {(_:util.LinkedHashMap[String, Object]).containsKey("address_zip")}
  }
}