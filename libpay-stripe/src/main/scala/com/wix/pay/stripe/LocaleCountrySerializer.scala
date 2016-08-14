/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2016, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.stripe


import java.util.Locale
import org.json4s.JsonAST.JString
import org.json4s.reflect.TypeInfo
import org.json4s.{Formats, JsonDSL, MappingException, Serializer, _}


class LocaleCountrySerializer extends Serializer[Locale] {
  private val LocaleClass = classOf[Locale]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Locale] = {
    case (TypeInfo(LocaleClass, _), json) => json match {
      case JString(country) =>
        new Locale("", country)
      case x => throw new MappingException("Can't convert " + x + " to LocaleClass")
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case x: Locale =>
      import JsonDSL._
      x.getCountry
  }
}
