package com.wix.pay.stripe

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonStripeMerchantParser() extends StripeMerchantParser {
  implicit val formats = DefaultFormats

  override def parse(merchantKey: String): StripeMerchant = {
    Serialization.read[StripeMerchant](merchantKey)
  }

  override def stringify(merchant: StripeMerchant): String = {
    Serialization.write(merchant)
  }
}
