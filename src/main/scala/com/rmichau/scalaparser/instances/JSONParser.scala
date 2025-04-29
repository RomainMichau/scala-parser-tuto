package com.rmichau.scalaparser.instances

import cats.syntax.functor.*
import com.rmichau.scalaparser.Combinators.*
import com.rmichau.scalaparser.ParserImplicit.given
import com.rmichau.scalaparser.Combinators.TupleSequencer.given 
import com.rmichau.scalaparser.*

object JSONParser {
  trait JObject extends PObject

  case class JString(value: String) extends JObject

  case class JKey(key: String) extends JObject

  case class JElem(key: String, value: JObject) extends JObject

  case class JArray(seq: Seq[JObject]) extends JObject

  case class JDict(map: Map[JKey, JObject]) extends JObject

  case class JBool(value: Boolean) extends JObject

  case class JInt(value: Int) extends JObject

  val COMMA: Parser[PString] = SYMBOL(",")
  val OBRACE: Parser[PString] = SYMBOL("{")
  val CBRACE: Parser[PString] = SYMBOL("}")
  val CSOBRACE: Parser[PString] = SYMBOL("[")
  val CSQBRACE: Parser[PString] = SYMBOL("]")
  val JKEY: Parser[JKey] = TUPLE(ANY_STRING(), SYMBOL(":")).map { (key, _) => JKey(key.value) }
  val JSTRING: Parser[JString] = ANY_STRING().map { case PString(value) => JString(value) }
  val STRING_COMMA: Parser[(JString, PString)] = TUPLE(JSTRING, COMMA)
  val JARRAY: Parser[(PString, POption, JString, PString)] = TUPLE(CSOBRACE, OPTION(LOOP(STRING_COMMA)), JSTRING, CSQBRACE)
  val teststuff = SEQ(JKEY, CSQBRACE)
  //  val string
  //  val DICT = SEQUENCE(Seq(CBRACE, KEY, ANY_OF()))
}
