package com.rmichau.scalaparser

import cats.data.Validated.{Invalid, Valid}
import cats.data.{ValidatedNec, ValidatedNel}
import com.rmichau.scalaparser.Combinators.*
import com.rmichau.scalaparser.TestCommon.*
import com.rmichau.scalaparser.instances.DateParser
import com.rmichau.scalaparser.instances.JSONParser.JString
import com.rmichau.scalaparser.{POption, PSeq, PString, ParseResult, Parser}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class ParserTest extends munit.FunSuite {

  def toTokens(st: String) = Lexer.tokenize(st)




  val tokens = Lexer.tokenize("\"hello\" \"world\" my name is Romain")
  val tokens2 = Lexer.tokenize("hello paris my name is Romain")
  val tokens3 = Lexer.tokenize("the book")
  val tokens4 = Lexer.tokenize("book")

  val hello = STRING("hello")
  val world = STRING("world")
  val the = IDENT("the")
  val book = IDENT("book")
  val helloWorld: Parser[PNeSeq] = SEQ(hello, world)
  val helloOrWorld: Parser[PString] = ANY_OF(Seq(hello, world))
  val optionalThe: Parser[POption] = OPTION(the)
  val theBook: Parser[PNeSeq] = SEQ(optionalThe, book)
  test("Test sequence") {
    assertSuccess(helloWorld(tokens, 0))
    assertFailure(helloWorld(tokens2, 0))
  }

  test("or") {
    assertSuccess(helloOrWorld(tokens, 0))
    assertSuccess(helloOrWorld(tokens, 1))
    assertFailure(helloOrWorld(tokens, 3))
  }

  test("optionnal") {
    assertSuccess(theBook(Lexer.tokenize("the book"), 0), 2)
    assertSuccess(theBook(Lexer.tokenize("book"), 0), 1)
    assertFailure(theBook(Lexer.tokenize("stuff"), 0))
  }

  test("eos") {
    assertSuccess(EOS(tokens3, 2))
    assertFailure(EOS(tokens3, 1))
  }

  test("ident") {
    assertSuccess(IDENT("hello")(Lexer.tokenize("hello my name is"), 0))
    assertFailure(IDENT("hello")(Lexer.tokenize("\"hello\" my name is"), 0))
  }

  test("any String") {
    val res = ANY_STRING()(Lexer.tokenize("\"what\" time is it"), 0)
    assertSuccess(res)
    assertResult(res, PString("what"))

    assertFailure(ANY_STRING()(Lexer.tokenize("what time is it"), 0))
  }

  test("symbol") {
    assertSuccess(SYMBOL("/")(Lexer.tokenize("/ 1213"), 0))
    assertFailure(SYMBOL("/")(Lexer.tokenize("/ 1213"), 3))
  }

  test("date") {
    val validDate = Lexer.tokenize("31 / jan / 1996")
    val res = DateParser.parse(validDate, 0)
    assertSuccess(DateParser.parse(validDate, 0))
  }

  test("tuple") {
    val hello = IDENT("hello")
    val maybeJohn = OPTION(IDENT("john"))
    val res: ParseResult[(PString, POption)] = TUPLE(hello, maybeJohn)(Lexer.tokenize("hello john"), 0)
    assertResult(res, (PString("hello"),POption(Some(PString("john")))))
  }
  
  test("loop") {
    val loopParser = LOOP(IDENT("hello"))
    val loopParser3 = LOOP(IDENT("hello"), 3)
    val loopParser4 = LOOP(IDENT("hello"), 4)
    assertSuccess(loopParser(tokens, 0))
    assertSuccess(loopParser3(toTokens("hello hello hello romain"), 0))
    assertFailure(loopParser4(toTokens("hello hello hello romain"), 0))
    val loopComplex = LOOP(SEQ(ANY_OF(Seq(IDENT("hello"), IDENT("hi"))),IDENT("romain")), 2)
    assertFailure(loopComplex(toTokens("hello romain"), 0))
    assertSuccess(loopComplex(toTokens("hello romain hello romain"), 0))
    assertSuccess(loopComplex(toTokens("hi romain hello romain hi romain"), 0))
    assertSuccess(loopComplex(toTokens("hi romain hello romain hi bro"), 0))
    assertFailure(loopComplex(toTokens("hi romain nope romain hi bro"), 0))
  }
}
