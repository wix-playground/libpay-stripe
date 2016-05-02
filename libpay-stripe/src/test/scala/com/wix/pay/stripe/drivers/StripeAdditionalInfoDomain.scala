package com.wix.pay.stripe.drivers

import java.util.Locale

import com.wix.pay.creditcard.{AddressDetailed, CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model._

trait StripeAdditionalInfoDomain {

  val someCcNumber = "4012888818888"
  val someCcExpiration = YearMonth(2020, 12)
  val someCcCsc = "123"
  val localeForUSA: Locale = new Locale("", "USA")
  val billingAddress = Some(AddressDetailed(
    street = Some("billingStreet"),
    city = Some("billingCity"),
    state = Some("billingState"),
    postalCode = Some("billingPostalCode"),
    countryCode = Some(localeForUSA))
  )
  val someCreditCard: CreditCard = CreditCard(
    number = someCcNumber,
    expiration = someCcExpiration,
    additionalFields = Some(CreditCardOptionalFields(
      csc = Some(someCcCsc)).withBillingAddressDetailed(billingAddress)))
  val someCustomer: Option[Customer] = Some(Customer(
    name = Some(Name("FirstName", "LastName")),
    phone = Some("phone"),
    email = Some("email"),
    ipAddress = Some("ipAddress"),
    fax = Some("fax"),
    company = Some("company")))
  val someInvoiceId = Some("invoiceId")
  val someShippingAddress: Some[ShippingAddress] = Some(ShippingAddress(
    firstName = Some("shippingFirstName"),
    lastName = Some("shippingLastName"),
    company = Some("shippingCompany"),
    phone = Some("phone"),
    email = Some("email"),
    fax = Some("fax"),
    address = Some(AddressDetailed(
      street = Some("shippingStreet"),
      city = Some("shippingCity"),
      state = Some("shippingState"),
      postalCode = Some("shippingPostalCode"),
      countryCode = Some(localeForUSA)
    ))
  ))
  val orderItems: Seq[OrderItem] = Seq(OrderItem(
    id = Some("orderItemId"),
    name = Some("OrderItemName"),
    quantity = Some(1.0),
    pricePerItem = Some(2.0),
    description = Some("ItemDescription")
  ),
    OrderItem(
      id = Some("orderItemId2"),
      name = Some("OrderItemName2"),
      quantity = Some(3.0),
      pricePerItem = Some(4.0),
      description = Some("ItemDescription2")
    ))

  val includedCharges = Some(IncludedCharges(tax = Some(23.03), shipping = Some(65.23)))

  val someDeal: Option[Deal] = Some(Deal("dealId",
    invoiceId = someInvoiceId,
    shippingAddress = someShippingAddress,
    orderItems = orderItems,
    includedCharges = includedCharges
  ))
}
