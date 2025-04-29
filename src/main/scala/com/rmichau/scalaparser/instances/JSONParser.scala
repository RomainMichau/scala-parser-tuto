package com.rmichau.scalaparser.instances

import com.rmichau.scalaparser.{Combinators, PNeSeq, PObject, Parser}
import Combinators.*
import cats.data.NonEmptySeq
import com.rmichau.scalaparser.ParserImplicit.given
import cats.syntax.functor.*
object JSONParser {
  trait JObject extends PObject
  case class JString(value: String) extends JObject
  case class JKey(key: String) extends JObject
  case class JElem(key: String, value: JObject) extends JObject
  case class JArray(seq: Seq[JObject]) extends JObject
  case class JDict(map: Map[JKey, JObject]) extends JObject
  case class JBool(value: Boolean) extends JObject
  case class JInt(value: Int) extends JObject

  val COMMA = SYMBOL(",")
  val OBRACE = SYMBOL("{")
  val CBRACE = SYMBOL("}")
  val KEY: Parser[JKey] = NESEQUENCE(NonEmptySeq.of(ANY_STRING(), SYMBOL(":"))).map{case PNeSeq(value) => JKey(value.head.toString)}
  
//  val string
//  val DICT = SEQUENCE(Seq(CBRACE, KEY, ANY_OF()))
}
