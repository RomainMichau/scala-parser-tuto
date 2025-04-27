package com.rmichau.scalaparser

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}

import scala.annotation.tailrec


object Main {
  def main(args: Array[String]): Unit = {
  }
}

type ParseResult[R] = ValidatedNec[String, (Int, R)]


case class Token(value: String) {
  override def toString: String = value
}

sealed trait PObject {
  def toPseq = PSeq(Seq(this))
}

case class PString(value: String) extends PObject

case class PInt(value: Int) extends PObject

case class PSeq(value: Seq[PObject]) extends PObject {
  def append(obj: PObject): PSeq = PSeq(value :+ obj)
}

case class POption(value: Option[PObject]) extends PObject

object PEOS extends PObject

case class Parser[R](f: (tokens: Seq[Token], idx: Int) => ParseResult[R], expecting: String) {
  def apply(tokens: Seq[Token], idx: Int): ParseResult[R] = f(tokens, idx)

  def map[R2 <: PObject](f: R => R2): Parser[R2] = ParserOps.map(this)(f)

  def flatMap[T <: PObject](transfo: R => Parser[T]): Parser[T] = ParserOps.flatMap(this)(transfo)
}

given ParserOps: cats.FlatMap[Parser] with {
  override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = {
    Parser((tokens: Seq[Token], idx: Int) => {
      fa(tokens, idx) match {
        case Valid((next, obj1)) => f(obj1)(tokens, next)
        case inv@Invalid(_) => inv
      }
    },
      expecting = fa.expecting)
  }

  override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = {
    Parser((tokens, idx) => {
      @tailrec
      def step(a: A, idx: Int): ParseResult[B] = {
        f(a)(tokens, idx) match {
          case Valid((nextIdx, Left(nextA))) => step(nextA, nextIdx)
          case Valid((nextIdx, Right(b))) => Valid((nextIdx, b))
          case i@Invalid(_) => i
        }
      }

      step(a, idx)
    }, expecting = "tailRecM")
  }

  override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = {
    Parser((tokens: Seq[Token], idx: Int) =>
      fa(tokens, idx).map { case (i, r) => (i, f(r)) },
      expecting = fa.expecting
    )
  }
}


extension (s: String) {
  def toToken: Token = Token(s)
}

object Combinators {
  private val tooShort = Validated.invalidNec(s"Input too short")

  def tokenise(input: String, splitter: String = " "): Seq[Token] = {
    input.toLowerCase.split(splitter).map(_.toToken)
  }

  // TODO
  // contravariant of word
  def INT(int: Int): Parser[PInt] = {
    Parser((tokens: Seq[Token], idx: Int) =>
      tokens.lift(idx) match {
        case Some(Token(w)) if w == int.toString => Validated.valid((idx + 1, PInt(w.toInt)))
        case _ => tooShort
      },
      expecting = s"int $int"
    )
  }

  def STRING(string: String): Parser[PString] = {
    Parser((tokens: Seq[Token], idx: Int) =>
      tokens.lift(idx) match {
        case Some(Token(w)) if w == string => Validated.valid((idx + 1, PString(w)))
        case Some(Token(w)) => Validated.invalidNec(s"Expected $string, got $w")
        case _ => tooShort
      },
      expecting = s"$string"
    )
  }

  def SEQUENCE(parsers: Seq[Parser[? <: PObject]]): Parser[PSeq] = {
    parsers match {
      case Seq() => Parser((tokens, idx) => Validated.valid((idx, PSeq(Seq()))), "empty sequence")
      case head +: tail =>
        tail.foldLeft(head.map(_.toPseq)) {
          (acc, parser) =>
            for {
              seq <- acc
              b <- parser
            } yield seq.append(b)
        }
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
        case inv@Invalid(_) => Invalid(NonEmptyChain(s"Not enough iteration of ${parser.expecting}. Expecting $minIteration iteration"))
      }
    }

    Parser((tokens, idx) => loop(tokens, idx, Nil, minIteration), expecting = s"Loop of ${parser.expecting}")
  }


  // TODO opimize
  def ANY_OF[T <: PObject](parsers: Seq[Parser[T]]): Parser[T] = {
    Parser((tokens: Seq[Token], idx: Int) => {
      parsers.find(p => p(tokens, idx).isValid) match {
        case Some(p) => p(tokens, idx)
        case None => Validated.invalidNec(s"Expecting one of ${parsers.map(_.expecting).mkString("/")}, got ${tokens.lift(idx).getOrElse("EOS")}")
      }
    },
      expecting = s"One of ${parsers.map(_.expecting).mkString("/")}")
  }

  def OPTION[T <: PObject](parser: Parser[T]): Parser[POption] = {
    Parser((tokens: Seq[Token], idx: Int) => {
      parser(tokens, idx) match {
        case Validated.Valid((next, v)) => Validated.valid((next, POption(Option(v))))
        case Validated.Invalid(_) => Validated.valid((idx, POption(None)))
      }
    },
      expecting = s"Optional ${parser.expecting}"
    )
  }

  def EOS[T]: Parser[Unit] = {
    Parser((tokens: Seq[Token], idx: Int) => {
      if (idx == tokens.length) Validated.valid((idx, ())) else Validated.invalidNec(s"expected EOS, got ${tokens(idx)}")
    },
      expecting = "EOS"
    )
  }
}

