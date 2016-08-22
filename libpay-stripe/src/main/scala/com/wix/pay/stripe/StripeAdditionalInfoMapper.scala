package com.wix.pay.stripe


import com.wix.pay.creditcard.{AddressDetailed, CreditCard}
import com.wix.pay.model._
import org.json4s.native.Serialization


class StripeAdditionalInfoMapper {

  def createMap(creditCard: CreditCard, customer: Option[Customer], deal: Option[Deal]): MappedParams = {
    val params = new MappedParams()

    creditCard.billingAddressDetailed.foreach(address => params.put("Billing Address", valueOf(address)))
    customer.foreach(customer => params.put("Customer", valueOf(customer)))

    deal.foreach(deal => extractInvoiceIdFrom(deal, intoParams = params))
    deal.foreach(deal => extractShippingAddressFrom(deal, intoParams = params))
    deal.foreach(deal => extractOrderItemsFrom(deal, intoParams = params))
    deal.foreach(deal => extractIncludedChargesFrom(deal, intoParams = params))

    params
  }

  private def valueOf(customer: Customer) = {
    Serialization.write(customer).adjustToStripeRequirements()
  }

  private def valueOf(billingAddress: AddressDetailed) = {
    Serialization.write(billingAddress).adjustToStripeRequirements()
  }

  private def extractInvoiceIdFrom(deal: Deal, intoParams: MappedParams) = {
    deal.invoiceId.foreach(id => intoParams.put("Invoice Id", id))
  }

  private def extractShippingAddressFrom(deal: Deal, intoParams: MappedParams): Unit = {
    deal.shippingAddress.foreach(address => intoParams.put("Shipping Address", valueOf(address)))
  }

  private def valueOf(shippingAddress: ShippingAddress) = {
    Serialization.write(shippingAddress).adjustToStripeRequirements()
  }

  private def extractOrderItemsFrom(deal: Deal, intoParams: MappedParams) = {
    if (deal.orderItems.nonEmpty)
      intoParams.put("Order Items", valueOf(deal.orderItems))
  }

  private def valueOf(orderItems: Seq[OrderItem]) = {
    Serialization.write(orderItems).adjustToStripeRequirements()
  }

  private def extractIncludedChargesFrom(deal: Deal, intoParams: MappedParams): Unit = {
    deal.includedCharges.foreach(includedCharges => intoParams.put("Included Charges", valueOf(includedCharges)))
  }

  private def valueOf(includedCharges: IncludedCharges) = {
    Serialization.write(includedCharges).adjustToStripeRequirements()
  }
}
