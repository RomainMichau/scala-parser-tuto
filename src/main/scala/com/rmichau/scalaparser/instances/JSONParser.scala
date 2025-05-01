//package com.rmichau.scalaparser.instances
//
//import cats.syntax.functor.*
//import com.rmichau.scalaparser.Combinators.*
//import com.rmichau.scalaparser.*
//import com.rmichau.scalaparser.ParserImplicit.given
//
//object JSONParser {
//  trait JObject
//
//  case class JString(value: String) extends JObject
//
//  case class JKey(key: String) extends JObject
//
//  case class JElem(key: String, value: JObject) extends JObject
//
//  case class JArray(seq: Seq[JObject]) extends JObject
//
//  case class JDict(map: Map[JKey, JObject]) extends JObject
//
//  case class JBool(value: Boolean) extends JObject
//
//  case class JInt(value: Int) extends JObject
//
//  val COMMA: ParserStE[PString]                   = SYMBOL(",")
//  val OBRACE: ParserStE[PString]                  = SYMBOL("{")
//  val CBRACE: ParserStE[PString]                  = SYMBOL("}")
//  val CSOBRACE: ParserStE[PString]                = SYMBOL("[")
//  val CSQBRACE: ParserStE[PString]                = SYMBOL("]")
//  val JKEY: ParserStE[JKey]                       = TUPLE(ANY_STRING(), SYMBOL(":")).map { (key, _) => JKey(key.value) }
//  val JSTRING: ParserStE[JString]                 = ANY_STRING().map { case PString(value) => JString(value) }
//  val STRING_COMMA: ParserStE[(JString, PString)] = TUPLE(JSTRING, COMMA)
////  val JARRAY = TUPLE(CSOBRACE, OPTION(REPEAT(STRING_COMMA)), JSTRING, CSQBRACE).map((_, b, c, _) => {
////    JArray(b.value.map(seq => seq.value.map((v1, _) => v1)).getOrElse(Seq()) :+ c)
////  })
//}
