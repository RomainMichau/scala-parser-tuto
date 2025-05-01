package com.rmichau.scalaparser

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, NonEmptySeq, Validated, ValidatedNec}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.rmichau.scalaparser.ParserImplicit.given

import scala.annotation.tailrec

type ParseResult[E, R] = ValidatedNec[E, (Int, R)]

type ParseResultSt[R] = ValidatedNec[String, (Int, R)]
type ParserStE[R]     = Parser[String, R]

case class Parser[E, R](f: (String, Int) => ParseResult[E, R], expecting: String) {
  def apply(str: String, idx: Int = 0): ParseResult[E, R] = f(str, idx)
}

object ParserImplicit {
  given ParserOps[E]: cats.Monad[[R] =>> Parser[E, R]]
    with cats.Semigroupal[[R] =>> Parser[E, R]]
    with cats.Bifunctor[Parser]
    with {
    override def pure[A](x: A): Parser[E, A] = Parser((_: String, idx: Int) => Valid((idx, x)), "pure parser")

    override def flatMap[R, R2](fa: Parser[E, R])(f: R => Parser[E, R2]): Parser[E, R2] = {
      Parser(
        (tokens: String, idx: Int) => {
          fa(tokens, idx) match {
            case Valid((next, obj1)) => f(obj1)(tokens, next)
            case inv @ Invalid(_)    => inv
          }
        },
        expecting = fa.expecting
      )
    }

    override def tailRecM[A, B](a: A)(f: A => Parser[E, Either[A, B]]): Parser[E, B] = {
      Parser(
        (tokens, idx) => {
          @tailrec
          def step(a: A, idx: Int): ParseResult[E, B] = {
            f(a)(tokens, idx) match {
              case Valid((nextIdx, Left(nextA))) => step(nextA, nextIdx)
              case Valid((nextIdx, Right(b)))    => Valid((nextIdx, b))
              case i @ Invalid(_)                => i
            }
          }

          step(a, idx)
        },
        expecting = "tailRecM"
      )
    }

    override def bimap[E1, R1, E2, R2](parser: Parser[E1, R1])(g: E1 => E2, f: R1 => R2): Parser[E2, R2] = {
      Parser(
        (tokens, idx) => {
          parser(tokens, idx).bimap(nec => nec.map(g), (idx, res) => (idx, f(res)))
        },
        "bimap"
      )
    }
  }
}

object Combinators {

  // TODO
  // contravariant of word

  def CHAR(char: Char): ParserStE[Char] = {
    Parser(
      (str: String, idx: Int) =>
        str.lift(idx) match {
          case Some(w) if w == char => Validated.valid((idx + 1, w))
          case Some(w)              => Validated.invalidNec(s"Missing CHAR($char) in $str, got $w instead")
          case _                    => Validated.invalidNec(s"Input too short")
        },
      expecting = s"char $char"
    )
  }

  def CHAR_IN(chars: Set[Char]): ParserStE[Char] = {
    Parser(
      (str: String, idx: Int) =>
        str.lift(idx) match {
          case Some(w) if chars.contains(w) => Validated.valid((idx + 1, w))
          case Some(w)                      => Validated.invalidNec(s"Missing $chars in $str, got $w instead")
          case _                            => Validated.invalidNec(s"Input too short")
        },
      expecting = s"chars in $chars"
    )
  }

  // Consume all WS and return a single one
  def WS1: ParserStE[Char] = REPEAT(ANY_OF(" \t\r\n".map(c => CHAR(c)))).as(' ')
  // Consume all WS and return a string of all
  def WSS: ParserStE[String] = REPEAT(ANY_OF(" \t\r\n".map(c => CHAR(c)))).map(_.mkString)

  def ALPHA(): ParserStE[Char] = {
    ANY_OF((('a' to 'z') ++ ('A' to 'Z')).map(CHAR))
  }

  def DIGIT(): ParserStE[Char] = ANY_OF(('0' to '9').map(CHAR))

  def WORD(): ParserStE[String] = REPEAT(ANY_OF(Seq(ALPHA(), DIGIT()))).map(_.mkString)

  def WORD(string: String): ParserStE[String] =
    SEQ(string.toCharArray.map(CHAR).head, string.toCharArray.map(CHAR).tail*).map(_.toSeq.mkString)

  // catch WORDs and whitespaces that are after
  def WORD_WS(): ParserStE[String]             = SEQ(WORD(), WSS).map(_.toSeq.mkString)
  def WORD_WS(word: String): ParserStE[String] = SEQ(WORD(word), WSS).map(_.toSeq.mkString)

  def WORD_WS0(): ParserStE[String]             = BEFORE(WORD(), WSS).map(_.toSeq.mkString)
  def WORD_WS0(word: String): ParserStE[String] = BEFORE(WORD(word), WSS).map(_.toSeq.mkString)

  def WORDS(): ParserStE[String]                       = REPEAT(WORD_WS()).map(_.mkString)
  def QUOTED_STRING()                                  = BETWEEN(CHAR('"'), WORDS(), CHAR('"'))
  def QUOTED_STRING(string: String): ParserStE[String] = BETWEEN(CHAR('"'), WORD(string), CHAR('"'))

  def SEQ[T, B](head: ParserStE[? <: T], tail: ParserStE[? <: T]*): ParserStE[NonEmptySeq[T]] = {
    tail.foldLeft(head.map(NonEmptySeq.one)) { (acc, parser) =>
      for {
        seq <- acc
        b   <- parser
      } yield seq.append(b)
    }
  }

  def REPEAT_SEPARATOR[T, T2](
      parser: ParserStE[T],
      separator: ParserStE[T2],
      minIteration: Int = 0
  ): ParserStE[Seq[T]] = {

    @tailrec
    def loop(tokens: String, idx: Int, acc: List[T], remainingMin: Int): ParseResult[String, Seq[T]] = {
      parser(tokens, idx) match {
        case Valid((nextIdx, value)) =>
          separator(tokens, nextIdx) match {
            case Valid((sepNextIdx, _)) =>
              loop(tokens, sepNextIdx, value :: acc, remainingMin - 1)
            case Invalid(_) if remainingMin <= 0 =>
              Valid((nextIdx, (value :: acc).reverse))
            case Invalid(_) =>
              Invalid(NonEmptyChain.one(s"Expected at least $minIteration repetitions of ${parser.expecting}"))
          }

        case Invalid(_) if remainingMin <= 0 =>
          Valid((idx, acc.reverse))

        case Invalid(_) =>
          Invalid(NonEmptyChain.one(s"Expected at least $minIteration repetitions of ${parser.expecting}"))
      }
    }

    Parser((tokens, idx) => loop(tokens, idx, Nil, minIteration), expecting = s"loop of ${parser.expecting}")
  }

  def REPEAT[T](parser: ParserStE[T], minIteration: Int = 0): ParserStE[Seq[T]] = {
    Parser(
      (tokens, startIdx) => {
        @tailrec
        def loop(idx: Int, acc: List[T], remainingMin: Int): ParseResult[String, Seq[T]] = {
          parser(tokens, idx) match {
            case Valid((nextIdx, value)) =>
              if (nextIdx == idx) {
                // Prevent infinite loop
                Valid((idx, acc.reverse))
              } else {
                loop(nextIdx, value :: acc, remainingMin - 1)
              }
            case Invalid(_) if remainingMin <= 0 =>
              Valid((idx, acc.reverse))
            case inv @ Invalid(_) =>
              inv
          }
        }
        loop(startIdx, Nil, minIteration)
      },
      expecting = s"Loop of ${parser.expecting}"
    )
  }

  // TODO opimize
  def ANY_OF[T](parsers: Seq[ParserStE[T]]): ParserStE[T] = {
    Parser(
      (tokens: String, idx: Int) => {
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

  def OPTION[T](parser: ParserStE[T]): ParserStE[Option[T]] = {
    Parser(
      (tokens: String, idx: Int) => {
        parser(tokens, idx) match {
          case Validated.Valid((next, v)) => Validated.valid((next, Option(v)))
          case Validated.Invalid(_)       => Validated.valid((idx, None))
        }
      },
      expecting = s"Optional ${parser.expecting}"
    )
  }

  def EOS: ParserStE[Unit] = {
    Parser(
      (tokens: String, idx: Int) => {
        if (idx == tokens.length) Validated.valid((idx, ()))
        else Validated.invalidNec(s"expected EOS, got ${tokens(idx)}")
      },
      expecting = "EOS"
    )
  }

  import cats.syntax.semigroupal.*

  def TUPLE[T1, T2](p1: ParserStE[T1], p2: ParserStE[T2]): ParserStE[(T1, T2)] = {
    p1 product p2
  }

  def BETWEEN[T1, T2, T3](p1: ParserStE[T1], p2: ParserStE[T2], p3: ParserStE[T3]): ParserStE[T2] = for {
    _   <- p1
    res <- p2
    _   <- p3
  } yield res

  def BEFORE[T1, T2](before: ParserStE[T1], after: ParserStE[T2]): ParserStE[T1] = for {
    r1 <- before
    _  <- after
  } yield r1
}
