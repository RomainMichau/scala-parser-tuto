package com.rmichau.scalaparser

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, NonEmptySeq, Validated, ValidatedNec}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {}
}

type ParseResult[R] = ValidatedNec[String, (Int, R)]

trait PObject {
  def toPseq = PSeq(Seq(this))

  def toPNeseq = PNeSeq(NonEmptySeq.one(this))
}

case class PString(value: String) extends PObject

case class PInt(value: Int) extends PObject

case class PSeq(value: Seq[PObject]) extends PObject {
  def append(obj: PObject): PSeq = PSeq(value :+ obj)
}

case class PNeSeq(value: NonEmptySeq[PObject]) extends PObject {
  def append(obj: PObject): PNeSeq = PNeSeq(value :+ obj)
}

case class POption(value: Option[PObject]) extends PObject

object PEOS extends PObject

case class Parser[R](f: (tokens: Seq[Token], idx: Int) => ParseResult[R], expecting: String) {
  def apply(tokens: Seq[Token], idx: Int): ParseResult[R] = f(tokens, idx)
}

object ParserImplicit {
  given ParserOps: cats.Monad[Parser] with {
    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = {
      Parser(
        (tokens: Seq[Token], idx: Int) => {
          fa(tokens, idx) match {
            case Valid((next, obj1)) => f(obj1)(tokens, next)
            case inv@Invalid(_) => inv
          }
        },
        expecting = fa.expecting
      )
    }

    override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = {
      Parser(
        (tokens, idx) => {
          @tailrec
          def step(a: A, idx: Int): ParseResult[B] = {
            f(a)(tokens, idx) match {
              case Valid((nextIdx, Left(nextA))) => step(nextA, nextIdx)
              case Valid((nextIdx, Right(b))) => Valid((nextIdx, b))
              case i@Invalid(_) => i
            }
          }

          step(a, idx)
        },
        expecting = "tailRecM"
      )
    }

    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = {
      Parser(
        (tokens: Seq[Token], idx: Int) => fa(tokens, idx).map { case (i, r) => (i, f(r)) },
        expecting = fa.expecting
      )
    }

    override def pure[A](x: A): Parser[A] = Parser(
      (tokens: Seq[Token], idx: Int) => Valid((idx, x)),
      "pure parser")
  }
}

object Combinators {
  private val tooShort = Validated.invalidNec(s"Input too short")

  // TODO
  // contravariant of word
  def INT(int: Int): Parser[PInt] = {
    Parser(
      (tokens: Seq[Token], idx: Int) =>
        tokens.lift(idx) match {
          case Some(TInt(w)) if w == int => Validated.valid((idx + 1, PInt(w)))
          case _ => tooShort
        },
      expecting = s"int $int"
    )
  }

  def STRING(string: String): Parser[PString] = {
    Parser(
      (tokens: Seq[Token], idx: Int) =>
        tokens.lift(idx) match {
          case Some(TString(w)) if w == string => Validated.valid((idx + 1, PString(w)))
          case Some(w) => Validated.invalidNec(s"Expected $string, got $w")
          case _ => tooShort
        },
      expecting = s"$string"
    )
  }

  def ANY_STRING(): Parser[PString] = {
    Parser(
      (tokens: Seq[Token], idx: Int) =>
        tokens.lift(idx) match {
          case Some(TString(w)) => Validated.valid((idx + 1, PString(w)))
          case _ => tooShort
        },
      expecting = s"Any string"
    )
  }

  def IDENT(ident: String): Parser[PString] = {
    Parser(
      (tokens: Seq[Token], idx: Int) =>
        tokens.lift(idx) match {
          case Some(TIdent(w)) if w == ident => Validated.valid((idx + 1, PString(w)))
          case Some(w) => Validated.invalidNec(s"Expected IDENT $ident, got $w")
          case _ => tooShort
        },
      expecting = s"$ident"
    )
  }

  def SYMBOL(symbol: String): Parser[PString] = {
    Parser(
      (tokens: Seq[Token], idx: Int) =>
        tokens.lift(idx) match {
          case Some(TSymbol(w)) if w == symbol => Validated.valid((idx + 1, PString(w)))
          case Some(w) => Validated.invalidNec(s"Expected $symbol, got $w")
          case _ => tooShort
        },
      expecting = s"$symbol"
    )
  }

  import ParserImplicit.given

  def SEQ(head: Parser[? <: PObject], tail: Parser[? <: PObject]*): Parser[PNeSeq] = {
    tail.foldLeft(head.map(_.toPNeseq)) { (acc, parser) =>
      for {
        seq <- acc
        b <- parser
      } yield seq.append(b)
    }
  }

  def LOOP[T <: PObject](parser: Parser[T], minIteration: Int = 0): Parser[PSeq] = {
    @tailrec
    def loop(tokens: Seq[Token], idx: Int, acc: List[PObject], roundToMakeMin: Int): ParseResult[PSeq] = {
      parser(tokens, idx) match {
        case Valid((nextIdx, value)) =>
          loop(tokens, nextIdx, value :: acc, roundToMakeMin - 1)
        case Invalid(e) if roundToMakeMin <= 0 =>
          Valid((idx, PSeq(acc.reverse)))
        case inv@Invalid(_) =>
          Invalid(NonEmptyChain(s"Not enough iteration of ${parser.expecting}. Expecting $minIteration iteration"))
      }
    }
    Parser((tokens, idx) => loop(tokens, idx, Nil, minIteration), expecting = s"Loop of ${parser.expecting}")
  }

  // TODO opimize
  def ANY_OF[T <: PObject](parsers: Seq[Parser[T]]): Parser[T] = {
    Parser(
      (tokens: Seq[Token], idx: Int) => {
        parsers.find(p => p(tokens, idx).isValid) match {
          case Some(p) => p(tokens, idx)
          case None =>
            Validated.invalidNec(
              s"Expecting one of ${parsers.map(_.expecting).mkString("/")}, got ${tokens.lift(idx).getOrElse("EOS")}"
            )
        }
      },
      expecting = s"One of ${parsers.map(_.expecting).mkString("/")}"
    )
  }

  def OPTION[T <: PObject](parser: Parser[T]): Parser[POption] = {
    Parser(
      (tokens: Seq[Token], idx: Int) => {
        parser(tokens, idx) match {
          case Validated.Valid((next, v)) => Validated.valid((next, POption(Option(v))))
          case Validated.Invalid(_) => Validated.valid((idx, POption(None)))
        }
      },
      expecting = s"Optional ${parser.expecting}"
    )
  }

  def EOS[T]: Parser[Unit] = {
    Parser(
      (tokens: Seq[Token], idx: Int) => {
        if (idx == tokens.length) Validated.valid((idx, ()))
        else Validated.invalidNec(s"expected EOS, got ${tokens(idx)}")
      },
      expecting = "EOS"
    )
  }

  def TUPLE[Parsers <: Tuple, Results <: Tuple](parsers: Parsers)(
    using seq: TupleSequencer[Parsers, Results]
  ): Parser[Results] = seq.apply(parsers)


  trait TupleSequencer[Parsers <: Tuple, Results <: Tuple] {
    def apply(parsers: Parsers): Parser[Results]
  }

  object TupleSequencer {

    import cats.syntax.apply.*

    given empty: TupleSequencer[EmptyTuple, EmptyTuple] {
      def apply(parsers: EmptyTuple): Parser[EmptyTuple] = EmptyTuple.pure
    }

    given cons[H <: PObject, T <: Tuple, RT <: Tuple](using
                                                      tail: TupleSequencer[T, RT]
                                                     ): TupleSequencer[Parser[H] *: T, H *: RT] with
      def apply(parsers: Parser[H] *: T): Parser[H *: RT] = {
        val head = parsers.head
        val tailParsed = tail(parsers.tail)
        (head, tailParsed).mapN(_ *: _)
      }

  }
}



