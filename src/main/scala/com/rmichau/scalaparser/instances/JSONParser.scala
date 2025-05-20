package com.rmichau.scalaparser.instances

import cats.syntax.functor.*
import com.rmichau.scalaparser.Combinators.*
import com.rmichau.scalaparser.*
import com.rmichau.scalaparser.ParserImplicit.given

object JSONParser {
  trait JObject

  case class JSTRING(value: String) extends JObject

  case class JKEY(key: String) extends JObject

  case class JElem(key: String, value: JObject) extends JObject

  case class JArray(seq: Seq[JObject]) extends JObject

  case class JDict(map: Map[JKEY, JObject]) extends JObject

  case class JBool(value: Boolean) extends JObject

  case class JInt(value: Int) extends JObject

  def defer[E, R](thunk: => Parser[E, R]): Parser[E, R] =
    Parser((str, idx) => thunk.f(str, idx), thunk.expecting)


  val keyP: ParserStE[JKEY]              = BEFORE(QUOTED_STRING(), WORD_WS0(":")).map(s => JKEY(s))
  val stringP: ParserStE[JSTRING]        = QUOTED_STRING().map(s => JSTRING(s))
  val objectP = ONE_OF[JObject](Seq(stringP, defer(arrayP), defer(dictP)))

  lazy val entryP: ParserStE[(JKEY, JObject)] = TUPLE(keyP, objectP)
  lazy val arrayP: ParserStE[JArray] =
    BETWEEN(WORD_WS0("["), REPEAT_SEPARATOR(objectP, WORD_WS0(",")), WORD_WS0("[")).map(s => JArray(s))
  lazy val dictP  = BETWEEN(WORD_WS0("{"), REPEAT_SEPARATOR(entryP, WORD_WS0(",")), WORD_WS0("}")).map(s => JDict(s.toMap))
  lazy val Parserr = objectP

  val input =
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
      |""".stripMargin.replaceAll("\\s", "")

  def main(args: Array[String]): Unit = {
    println(Parserr(input))
  }
}


