package com.wix.pay.stripe

trait StripeMerchantParser {
  def parse(merchantKey: String): StripeMerchant
  def stringify(merchant: StripeMerchant): String
}
