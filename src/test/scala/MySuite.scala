import com.rmichau.scalaparser.Combinators.*
import cats.data.{ValidatedNec, ValidatedNel}
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import com.rmichau.scalaparser.instances.DateParser
import com.rmichau.scalaparser.{POption, PSeq, PString, ParseResult, Parser, Token}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {

  def toTokens(st: String) = st.split(" ").map(Token(_))

  def assertSuccess[R](result: ParseResult[R]): Unit = {
    result match
      case Valid(next) => assert(true)
      case Invalid(err) => assert(false, s"unexpected error: ${err.toNonEmptyList.toList.mkString("\n")}")
  }

  def assertSuccess[R](result: ParseResult[R], expectedNext: Int): Unit = {
    result match
      case Valid((next, _)) => assertEquals(next, expectedNext)
      case Invalid(err) => assert(false, s"unexpected error: $err")
  }

  def assertFailure[R](result: ParseResult[R]): Unit = {
    assert(result.isInvalid)
  }

  val tokens = tokenise("hello world my name is Romain")
  val tokens2 = tokenise("hello paris my name is Romain")
  val tokens3 = tokenise("the book")
  val tokens4 = tokenise("book")

  val hello = STRING("hello")
  val world = STRING("world")
  val the = STRING("the")
  val book = STRING("book")
  val helloWorld: Parser[PSeq] = SEQUENCE(Seq(hello, world))
  val helloOrWorld: Parser[PString] = ANY_OF(Seq(hello, world))
  val optionalThe: Parser[POption] = OPTION(the)
  val theBook: Parser[PSeq] = SEQUENCE(Seq(optionalThe, book))
  test("Test tokenize") {
    assertEquals(tokenise("Hello World"), Seq(Token("hello"), Token("world")))
  }
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
    assertSuccess(theBook(tokenise("the book"), 0), 2)
    assertSuccess(theBook(tokenise("book"), 0), 1)
    assertFailure(theBook(tokenise("stuff"), 0))
  }

  test("eos") {
    assertSuccess(EOS(tokens3, 2))
    assertFailure(EOS(tokens3, 1))
  }

  test("date") {
    val validDate = tokenise("31 / Jan / 1996")
    val res = DateParser.parse(validDate, 0)
    assertSuccess(DateParser.parse(validDate, 0))
  }

  test("loop") {
    val loopParser = LOOP(STRING("hello"))
    val loopParser3 = LOOP(STRING("hello"), 3)
    val loopParser4 = LOOP(STRING("hello"), 4)
    assertSuccess(loopParser(tokens, 0))
    assertSuccess(loopParser3(toTokens("hello hello hello romain"), 0))
    assertFailure(loopParser4(toTokens("hello hello hello romain"), 0))
    val loopComplex = LOOP(SEQUENCE(Seq(ANY_OF(Seq(STRING("hello"), STRING("hi"))),STRING("romain"))), 2)
    assertFailure(loopComplex(toTokens("hello romain"), 0))
    assertSuccess(loopComplex(toTokens("hello romain hello romain"), 0))
    assertSuccess(loopComplex(toTokens("hi romain hello romain hi romain"), 0))
    assertSuccess(loopComplex(toTokens("hi romain hello romain hi bro"), 0))
    assertFailure(loopComplex(toTokens("hi romain nope romain hi bro"), 0))
  }
}
