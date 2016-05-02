package com.wix.pay

import java.util

package object stripe {

  type MappedParams = util.LinkedHashMap[String, Object]

  implicit class `String --> slicedString`(jsonStr: String) {
    final val ValueCharacterLimit = 500

    def adjustToStripeRequirements() = {
      jsonStr.slice(0, ValueCharacterLimit)
    }
  }
}
