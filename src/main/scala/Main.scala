
object Main {
  def main(args: Array[String]): Unit = {
  }
}

case class Parser[T](f: (tokens: Seq[Token[T]], idx: Int) => Result) {
  def apply(tokens: Seq[Token[T]], idx: Int) = f(tokens, idx)

  def andThen(p2: Parser[T]): Parser[T] = {
    Parser((tokens: Seq[Token[T]], idx: Int) =>
      f(tokens, idx) match {
        case Success(next) => p2.f(tokens, next)
        case _ => Failure()
      })
  }
}

sealed trait Result {
  def isSuccess: Boolean
}

case class Success(next: Int) extends Result {
  override def isSuccess: Boolean = true
}

case class Failure() extends Result {
  override def isSuccess: Boolean = false
}

case class Token[T](value: T)

type TString = Token[String]

extension (s: String) {
  def toToken: Token[String] = Token(s)
}

object Parser {
  def tokenise(input: String, splitter: String = " "): Seq[TString] = {
    input.toLowerCase.split(splitter).map(_.toToken)
  }

  // TODO
  // contravariant of word
  def int(int: Int): Parser[Int] = {
    Parser((tokens: Seq[Token[Int]], idx: Int) =>
      tokens.lift(idx) match {
        case Some(Token(w)) if w == int => Success(idx + 1)
        case _ => Failure()
      })
  }
  
  def word(word: String): Parser[String] = {
    Parser((tokens: Seq[TString], idx: Int) =>
      tokens.lift(idx) match {
        case Some(Token(w)) if w == word => Success(idx + 1)
        case _ => Failure()
      })
  }

  def sequence[T](parsers: Seq[Parser[T]]): Parser[T] = {
    parsers.reduce((p1, p2) => {
      p1.andThen(p2)
    })
  }

  // TODO opti
  def anyOf[T](parsers: Seq[Parser[T]]): Parser[T] = {
    Parser((tokens: Seq[Token[T]], idx: Int) => {
      parsers.find(p => p(tokens, idx).isSuccess) match {
        case Some(p) => p(tokens, idx)
        case None => Failure()
      }
    })
  }
  
  def optional[T](parser: Parser[T]): Parser[T] = {
    Parser((tokens: Seq[Token[T]], idx: Int) => {
        parser(tokens, idx) match {
          case Success(next) => Success(next)
          case Failure() => Success(idx)
        }
    })
  }
  
  def eos[T]: Parser[T] = {
    Parser((tokens: Seq[Token[T]], idx: Int) => {
      if(idx == tokens.length) Success(idx) else Failure()
    })
  }
}

object DateParser {
  import Parser._
  val domParsers = (1 until 31).map(_.toString).map(word)
  val dom = anyOf(domParsers)
  val monthParsers = Seq("Jan", "Feb", "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec").map(word)
  val month = anyOf(monthParsers)
  val yearsParser = (0 until 3000).map(_.toString).map(word)
  val year = anyOf(yearsParser)
  val slachOrDash = anyOf(Seq(word("/"), word("-")))
  val dateParser = sequence(Seq(dom, slachOrDash, month, slachOrDash, year))
  def parse = dateParser
}