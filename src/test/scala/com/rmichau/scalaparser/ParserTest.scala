package com.rmichau.scalaparser

import com.rmichau.scalaparser.Combinators.*
import com.rmichau.scalaparser.Parsers.*
import com.rmichau.scalaparser.TestCommon.*

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class ParserTest extends munit.FunSuite {

  def toTokens(st: String) = (st)

  val tokens2 = "hello paris my name is Romain"

  test("char") {
    CHAR('h')("hello").assertSuccess('h')
    CHAR('e')("hello").assertFailure()
    CHAR_IN(Set('a', 'c', 'h'))("hello").assertSuccess('h')
    CHAR_IN(Set('a', 'c', 'h'))("bye").assertFailure()
    CHAR_IN(Set('a', 'c', 'h'))("").assertFailure()
  }

  test("ALHPA") {
    ALPHA()("hello").assertSuccess('h')
  }

  test("WS") {
    WS1("  a  lot of      whitespace").assertSuccess(' ')
  }

  test("WORD") {
    WORD("hello")("hello world").assertSuccess("hello")
    WORD("he")("hello world").assertSuccess("he")
    WORD()("hello world").assertSuccess("hello")
    WORD_WS()("hello   world").assertSuccess("hello   ")
    WORD_WS("the")("the hello from  earth").assertSuccess()
  }

  test("WORD_WS0") {
    WORD_WS0("hello")("hello         world").assertSuccess("hello")
    WORD_WS0("hi")("hello         world").assertFailure()
    WORD_WS0("hello")("oops").assertFailure()

    WORD_WS0()("hello").assertSuccess()
  }

  test("WORDS") {
    WORDS()("these  are several   words").assertSuccess("these  are several   words")
    println(WORDS().expecting)
  }

  test("BETWEEN") {
    BETWEEN(CHAR(';'), WORDS(), CHAR(','))(";hel lo,").assertSuccess("hel lo")
  }

  test("QUOTED STRING") {
    QUOTED_STRING()("\"this is a quoted string\"").assertSuccess("this is a quoted string")
  }

  test("Test sequence") {
    val hello      = QUOTED_STRING("hello")
    val world      = QUOTED_STRING("world")
    val helloWorld = SEQ(hello, world)
    helloWorld("\"hello\"\"world\" my name is Romain").assertSuccess()
    helloWorld(tokens2, 0).assertFailure()
  }

  test("or") {
    val helloOrWorld = ANY_OF(Seq(QUOTED_STRING("hello"), QUOTED_STRING("world")))
    helloOrWorld("\"hello\" \"world\" my name is Romain", 0).assertSuccess()
    helloOrWorld("\"hello\" \"world\" my name is Romain", 1).assertFailure()
    helloOrWorld("\"hello\" \"world\" my name is Romain", 3).assertFailure()
  }

  test("optionnal") {
    val the         = WORD_WS("the")
    val book        = WORD_WS("book")
    val optionalThe = OPTION(the)
    val theBook     = SEQ(optionalThe, book)

    theBook("the book").assertSuccess()
    theBook("book").assertSuccess()
    theBook("stuff").assertFailure()
  }

  test("eos") {
    EOS("the book", 8).assertSuccess()
    EOS("book", 2).assertFailure()
  }

  test("TUPLE") {
    val hello     = WORD_WS("hello")
    val maybeJohn = OPTION(WORD_WS("john"))
    val res       = TUPLE(hello, maybeJohn)("hello john", 0)
    res.assertSuccess(("hello ", Some("john")))
  }

  test("REPEAT") {
    val loopParser  = REPEAT(WORD_WS("hello"))
    val loopParser3 = REPEAT(WORD_WS("hello"), 3)
    val loopParser4 = REPEAT(WORD_WS("hello"), 4)
    loopParser("\"hello\" \"world\" my name is romain", 0).assertSuccess()
    loopParser3(toTokens("hello hello hello romain"), 0).assertSuccess()
    loopParser4(toTokens("hello hello hello romain"), 0).assertFailure()
    val loopComplex = REPEAT(SEQ(ANY_OF(Seq(WORD_WS("hello"), WORD_WS("hi"))), WORD_WS("romain")), 2)
    loopComplex(toTokens("hello romain"), 0).assertFailure()
    loopComplex(toTokens("hello romain hello romain"), 0).assertSuccess()
    loopComplex(toTokens("hi romain hello romain hi romain"), 0).assertSuccess()
    loopComplex(toTokens("hi romain hello romain hi bro"), 0).assertSuccess()
    loopComplex(toTokens("hi romain nope romain hi bro"), 0).assertFailure()
  }

  test("REPEAT_SEPARATOR") {
    val repeatParser = REPEAT_SEPARATOR(WORD("hello"), CHAR(','))
    repeatParser("hello,hello,hello").assertSuccess()
    repeatParser("hello,hi,hello").assertSuccess(Seq("hello"))

    val repeatParser2 = REPEAT_SEPARATOR(WORD("hello"), CHAR(','), 2)
    repeatParser2("hello,").assertFailure()
    repeatParser2("hi").assertFailure()
    repeatParser2("hello,hello").assertFailure()
  }

  test("tailRecM") {
    import cats.syntax.flatMap.*
    import cats.syntax.functor.*
    import com.rmichau.scalaparser.ParserImplicit.given
    val p = "1".tailRecM(st => {
      WORD(st).map(s => if (s.length < 5) Left(s :+ '1') else Right(s))
    })
    p("111111111111111111111111").assertSuccess()
    p("1").assertFailure()
  }

  test("bimap") {
    import cats.syntax.bifunctor.*
    import com.rmichau.scalaparser.ParserImplicit.given
    val p = WORD("hello").bimap(_ => "error TEST", _ => "success TEST")
    p("hi").assertFailure("error TEST")
    p("hello").assertSuccess("success TEST")
  }
}
