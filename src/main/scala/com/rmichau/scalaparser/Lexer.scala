package com.rmichau.scalaparser

import cats.data.State
import cats.implicits.catsSyntaxFlatMapOps
import com.rmichau.scalaparser.Lexer.Capturor

sealed trait Token

case class TString(value: String) extends Token // "hello"

case class TInt(value: Int) extends Token // 42

case class TIdent(name: String) extends Token // x, userName, if, else

case class TSymbol(symbol: String) extends Token // =, +, -, {, }

case object TEOF extends Token // End of stream

case class LexerState(
    tokens: Seq[Token],
    buffer: Seq[Char],
    capturor: Capturor
)

type Lexer[A] = State[LexerState, A]

object Lexer {
  private val EOF = '\u0000'

  def step(char: Char): State[LexerState, Unit] =
    State.modify { state =>
      val (newTokens, newBuffer, newCapturor) = state.capturor(char, state.tokens, state.buffer)
      LexerState(newTokens, newBuffer, newCapturor)
    }

  def tokenize(input: String): Seq[Token] = {
    val steps: Seq[Lexer[Unit]] = input.appended(EOF).toCharArray.map(step)
    val program: Lexer[Unit] = steps.foldLeft(State.pure[LexerState, Unit](())) { (acc, next) =>
      acc >> next
    }

    val initialState = LexerState(Seq.empty, Seq.empty, GeneralCapturor)

    val (finalState, _) = program.run(initialState).value
    finalState.tokens
  }

  private val symbols = ",<>+-*/=()[]{}!@#$%^&|~;:?".toSet
  private val space   = ' '

  private def isSymbol(char: Char): Boolean = symbols.contains(char)

  type CapturorRes = (Seq[Token], Seq[Char], Capturor)
  sealed trait Capturor {
    def apply(currentChar: Char, acc: Seq[Token], buffer: Seq[Char]): CapturorRes
  }

  private object NumberCapturor extends Capturor {
    override def apply(currentChar: Char, acc: Seq[Token], buffer: Seq[Char]): (Seq[Token], Seq[Char], Capturor) = {
      currentChar match {
        case c: Char if c.isDigit =>
          (acc, buffer :+ c, NumberCapturor)
        case c: Char if isSymbol(c) =>
          val token  = TInt(buffer.mkString.toInt)
          val newAcc = acc :+ token
          (newAcc :+ TSymbol(c.toString), Seq(), GeneralCapturor)
        case c: Char if c == space =>
          val token = TInt(buffer.mkString.toInt)
          (acc :+ token, Seq(), GeneralCapturor)
        case c: Char if c == EOF =>
          val token  = TInt(buffer.mkString.toInt)
          val newAcc = acc :+ token
          (newAcc :+ TEOF, Seq(), GeneralCapturor)
        case _ =>
          throw new Exception(s"Excepting digit or symbol after ${acc.toString()}")
      }
    }
  }

  private object IdentCapturor extends Capturor {
    override def apply(currentChar: Char, acc: Seq[Token], buffer: Seq[Char]): (Seq[Token], Seq[Char], Capturor) = {
      currentChar match {
        case c: Char if c.isLetterOrDigit    => (acc, buffer :+ c, IdentCapturor)
        case c: Char if c == ' ' || c == EOF => (acc :+ TIdent(buffer.mkString), Seq(), GeneralCapturor)
        case c: Char if isSymbol(c)          => (acc :+ TSymbol(c.toString), Seq(), GeneralCapturor)
        case _ => throw new Exception(s"Excepting digit/letter or space after ${acc.toString()}, got $currentChar")
      }
    }
  }

  private object StringCapturor extends Capturor {
    override def apply(currentChar: Char, acc: Seq[Token], buffer: Seq[Char]): (Seq[Token], Seq[Char], Capturor) = {
      currentChar match {
        case c: Char if c != '"' => (acc, buffer :+ c, StringCapturor)
        case c: Char if c == '"' => (acc :+ TString(buffer.mkString), Seq(), GeneralCapturor)
        case _                   => throw new Exception(s"Excepting char or \" ${acc.toString()}")
      }
    }
  }

  private object GeneralCapturor extends Capturor {
    override def apply(currentChar: Char, acc: Seq[Token], buffer: Seq[Char]): (Seq[Token], Seq[Char], Capturor) = {
      currentChar match {
        case c: Char if c.isLetter     => (acc, buffer :+ c, IdentCapturor)
        case c: Char if c.isDigit      => (acc, buffer :+ c, NumberCapturor)
        case c: Char if c == '"'       => (acc, buffer, StringCapturor)
        case c: Char if isSymbol(c)    => (acc :+ TSymbol(c.toString), Seq(), GeneralCapturor)
        case c: Char if c == EOF       => (acc :+ TEOF, Seq(), GeneralCapturor)
        case c: Char if c.isWhitespace => (acc, Seq(), GeneralCapturor)
        case c                         => throw new Exception(s"Unexpected char: ${c} after $acc")
      }
    }
  }
}
