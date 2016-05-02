package com.wix.pay.stripe.testkit

import com.stripe.Stripe

object StripeITEnvironment {
  final val StripePort = 9035

  def setUpStripeDriver() = {
    // Ugly, but seems to be the only way to do this
    Stripe.overrideApiBase(s"http://localhost:$StripePort")

    stripeDriver.startStripeProbe()
  }

  def tearDownStripeDriver() = {
    stripeDriver.stopStripeProbe()
  }

  val stripeDriver = new StripeDriver(port = StripePort)
}
