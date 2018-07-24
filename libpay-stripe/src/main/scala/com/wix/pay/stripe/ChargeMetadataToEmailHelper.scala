package com.wix.pay.stripe

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, OrderItem}

import scala.collection.immutable.ListMap

class ChargeMetadataToEmailHelper {
  def getMetadataForEmail(creditCard: CreditCard, customer: Option[Customer], deal: Option[Deal]): Map[String, String] = {

    val customerInfo = ListMap(
      "Billing Address" → billingAddressInfo(creditCard),
      "Customer Name" → customer.flatMap(customerNameInfo),
      "Customer Phone" → customer.flatMap(_.phone),
      "Customer Email" → customer.flatMap(_.email),
      "Customer IP" → customer.flatMap(_.ipAddress),
      "Invoice Id" → deal.flatMap(_.invoiceId),
      "Shipping Address" → deal.flatMap(shippingAddressInfo)
    )

    val includedChargesInfo = ListMap(
      "Included Charges: Tax" → deal.flatMap(chargeTaxInfo),
      "Included Charges: Shipping" → deal.flatMap(chargeShippingInfo)
    )

    val itemsInfo = orderItemsInfo(deal)

    val emailPairs = (customerInfo.toSeq ++ itemsInfo.toSeq ++ includedChargesInfo.toSeq).collect {
      case (k, Some(v)) ⇒ (k, v)
    }

    ListMap(emailPairs:_*)
  }

  private def orderItemsInfo(maybeDeal: Option[Deal]) = {
    val itemsInfo = for {
      deal ← maybeDeal.toSeq
      item ← deal.orderItems
      itemName ← item.name.toSeq
    } yield itemName → orderItemInfo(item)
    ListMap(itemsInfo:_*)
  }

  private def orderItemInfo(orderItem: OrderItem) = {
    val itemInfo = Seq(orderItem.id, orderItem.quantity, orderItem.pricePerItem).flatten.mkString(", ")
    Some(itemInfo).filter(_.nonEmpty)
  }

  private def billingAddressInfo(creditCard: CreditCard) =
    creditCard.additionalFields.flatMap(_.billingAddressDetailed).map { billingAddress ⇒
      import billingAddress._
      Seq(street, city, state, postalCode, countryCode.map(_.getCountry)).flatten.mkString(" ")
    }

  private def shippingAddressInfo(deal: Deal) =
    deal.shippingAddress.map { address ⇒
      import address._
      Seq(street, city, state, postalCode, countryCode.map(_.getCountry)).flatten.mkString(" ")
    }

  private def customerNameInfo(customer: Customer)= for {
    name ← customer.name
    firstName = name.first
    lastName = name.last
  } yield s"$firstName $lastName"

  private def chargeTaxInfo(deal: Deal) = for {
    includedCharges ← deal.includedCharges
    tax ← includedCharges.tax
  } yield tax.toString

  private def chargeShippingInfo(deal: Deal) = for {
    includedCharges ← deal.includedCharges
    shipping ← includedCharges.shipping
  } yield shipping.toString

}