package com.rmichau.scalaparser

import cats.data.Validated.{Invalid, Valid}
import cats.data.{ValidatedNec, ValidatedNel}
import com.rmichau.scalaparser.Combinators.*
import com.rmichau.scalaparser.instances.DateParser
import com.rmichau.scalaparser.*

class LexerTest extends munit.FunSuite {

  test("basic lexer") {
    val res = Lexer.tokenize("hello i am romain 123 \"hello friend\"")
    assertEquals(res, List(TIdent("hello"), TIdent("i"), TIdent("am"), TIdent("romain"), TInt(123), TString("hello friend"), TEOF))
    print(res)
  }

  test("json lexer") {
    val input =
      """
        |
        |
        |{
        |    "glossary": {
        |        "title": "example glossary",
        |		"GlossDiv": {
        |            "title": "S",
        |			"GlossList": {
        |                "GlossEntry": {
        |                    "ID": "SGML",
        |					"SortAs": "SGML",
        |					"GlossTerm": "Standard Generalized Markup Language",
        |					"Acronym": "SGML",
        |					"Abbrev": "ISO 8879:1986",
        |					"GlossDef": {
        |                        "para": "A meta-markup language, used to create markup languages such as DocBook.",
        |						"GlossSeeAlso": ["GML", "XML"]
        |                    },
        |					"GlossSee": "markup"
        |                }
        |            }
        |        }
        |    }
        |}
        |
        |""".stripMargin
    val res = Lexer.tokenize(input)
    assertEquals(res, List(TSymbol("{"), TString("glossary"), TSymbol(":"), TSymbol("{"), TString("title"), TSymbol(":"), TString("example glossary"), TSymbol(","), TString("GlossDiv"), TSymbol(":"), TSymbol("{"), TString("title"), TSymbol(":"), TString("S"), TSymbol(","), TString("GlossList"), TSymbol(":"), TSymbol("{"), TString("GlossEntry"), TSymbol(":"), TSymbol("{"), TString("ID"), TSymbol(":"), TString("SGML"), TSymbol(","), TString("SortAs"), TSymbol(":"), TString("SGML"), TSymbol(","), TString("GlossTerm"), TSymbol(":"), TString("Standard Generalized Markup Language"), TSymbol(","), TString("Acronym"), TSymbol(":"), TString("SGML"), TSymbol(","), TString("Abbrev"), TSymbol(":"), TString("ISO 8879:1986"), TSymbol(","), TString("GlossDef"), TSymbol(":"), TSymbol("{"), TString("para"), TSymbol(":"), TString("A meta-markup language, used to create markup languages such as DocBook."), TSymbol(","), TString("GlossSeeAlso"), TSymbol(":"), TSymbol("["), TString("GML"), TSymbol(","), TString("XML"), TSymbol("]"), TSymbol("}"), TSymbol(","), TString("GlossSee"), TSymbol(":"), TString("markup"), TSymbol("}"), TSymbol("}"), TSymbol("}"), TSymbol("}"), TSymbol("}"), TEOF))
  }

  test("Scala code") {
    val input =
      """
        |object HelloWorld {
        |  def main(args: Array[String]): Unit = {
        |    println("Hello, World!")
        |  }
        |}
        |""".stripMargin
    val res = Lexer.tokenize(input)
    print(res)
    assertEquals(res, List(TIdent("object"), TIdent("HelloWorld"), TSymbol("{"), TIdent("def"), TSymbol("("), TSymbol(":"), TSymbol("["), TSymbol("]"), TSymbol(")"), TSymbol(":"), TIdent("Unit"), TSymbol("="), TSymbol("{"), TSymbol("("), TString("Hello, World!"), TSymbol(")"), TSymbol("}"), TSymbol("}"), TEOF))
  }
}
