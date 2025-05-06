package com.rmichau.scalaparser.instances

import cats.syntax.functor.*
import com.rmichau.scalaparser.Combinators.*
import com.rmichau.scalaparser.*
import com.rmichau.scalaparser.ParserImplicit.given
import com.rmichau.scalaparser.Parsers.*
//
//object LazyParser {
//  def apply[R](): ParserImplStE[R] = new Parser[String, R] {
//
//}

object JSONParser {

  trait JObject

  case class JString(value: String) extends JObject

  case class JKey(key: String) extends JObject

  case class JElem(key: String, value: JObject) extends JObject

  case class JArray(seq: Seq[JObject]) extends JObject

  case class JDict(map: Map[JKey, JObject]) extends JObject

  case class JBool(value: Boolean) extends JObject

  case class JInt(value: Int) extends JObject

  val COMMA    = WORD_WS0(",")
  val OBRACE   = WORD_WS0("{")
  val CBRACE   = WORD_WS0("}")
  val OSQBRACE = WORD_WS0("[")
  val CSQBRACE = WORD_WS0("]")

  val key: ParserStE[JKey] = BEFORE(QUOTED_STRING(), WORD_WS0(":")).map(s => JKey.apply(s))
  val jstring              = QUOTED_STRING().map(JString.apply)

  val kv: ParserStE[(JKey, JObject)] = TUPLE(key, JObject)

  val jarray: ParserStE[JArray] =
    BETWEEN(OSQBRACE, REPEAT_SEPARATOR(JObject, COMMA), CSQBRACE).map(JArray.apply)

  val jdict: ParserStE[JDict] =
    BETWEEN(OBRACE, REPEAT_SEPARATOR(kv, COMMA), CBRACE).map(s => JDict.apply(s.toMap))
  lazy val JObject: ParserStE[JObject] = ANY_OF[JObject](Seq(jstring, jarray, jdict))
  val JSONParser                       = BETWEEN(BEGIN, JObject, EOS)
  def main(args: Array[String]): Unit = {
    val inp =
      """
        |{
        |  "name": "Alice",
        |  "status": "active",
        |  "tags": ["admin", "editor", "reviewer"],
        |  "profile": {
        |    "firstName": "Alice",
        |    "lastName": "Smith",
        |    "location": "Wonderland"
        |  }
        |}
        |
        |""".stripMargin.filterNot(_.isWhitespace)
    println(inp)
    println(JSONParser(inp))
  }
}
