import Parser.{sequence, tokenise, word}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {

  def assertSuccess(result: Result): Unit = {
    result match
      case Success(_) => assert(true)
      case _ => assert(false)
  }

  def assertSuccess(result: Result, expectedNext: Int): Unit = {
    result match
      case Success(next) => assertEquals(next, expectedNext)
      case _ => assert(false)
  }

  def assertFailure(result: Result): Unit = {
    result match
      case Failure() => assert(true)
      case _ => assert(false)
  }

  val tokens = Parser.tokenise("hello world my name is Romain")
  val tokens2 = Parser.tokenise("hello paris my name is Romain")
  val tokens3 = Parser.tokenise("the book")
  val tokens4 = Parser.tokenise("book")

  val hello = Parser.word("hello")
  val world = Parser.word("world")
  val the = Parser.word("the")
  val book = Parser.word("book")
  val helloWorld: Parser[String] = sequence(Seq(hello, world))
  val helloOrWorld: Parser[String] = Parser.anyOf(Seq(hello, world))
  val optionalThe: Parser[String] = Parser.optional(the)
  val theBook: Parser[String] = Parser.sequence(Seq(optionalThe, book))
  test("Test tokenize") {
    assertEquals(Parser.tokenise("Hello World"), Seq(Token("hello"), Token("world")))
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
    assertSuccess(Parser.eos(tokens3, 2))
    assertFailure(Parser.eos(tokens3, 1))
  }
  
  test("date") {
    val validDate = Parser.tokenise("31 - 01 - 1996")
    assertSuccess(DateParser.parse(validDate, 0))
  }
}
