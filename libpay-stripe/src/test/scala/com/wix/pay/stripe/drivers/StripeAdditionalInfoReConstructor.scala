package com.wix.pay.stripe.drivers

import com.wix.pay.creditcard.AddressDetailed
import com.wix.pay.model.{Customer, IncludedCharges, OrderItem, ShippingAddress}
import com.wix.pay.stripe.{CustomFormats, _}
import org.json4s.native.Serialization

object StripeAdditionalInfoReConstructor {
  implicit val formats = CustomFormats

  def reconstructBillingAddress(params: MappedParams) = {
    val address = params.get("Billing Address").toString

    Serialization.read[AddressDetailed](address)
  }

  def reconstructCustomer(params: MappedParams) = {
    val customer = params.get("Customer").toString

    Serialization.read[Customer](customer)
  }

  def reconstructInvoiceId(params: MappedParams) = {
    params.get("Invoice Id").toString
  }

  def reconstructShippingAddress(params: MappedParams): ShippingAddress = {
    val address = params.get("Shipping Address").toString

    Serialization.read[ShippingAddress](address)
  }

  def reconstructOrderItems(params: MappedParams) = {
    val orderItems = params.get("Order Items").toString

    Serialization.read[Seq[OrderItem]](orderItems)
  }

  def reconstructIncludedCharges(params: MappedParams) = {
    val includedCharges = params.get("Included Charges").toString

    Serialization.read[IncludedCharges](includedCharges)
  }
}
