package com.rmichau.scalaparser

import cats.data.Validated.{Invalid, Valid}
import munit.Assertions.assertEquals

object TestCommon {

  def assertResult[R](result: ParseResult[R], expected: R): Unit = {
    assert(result.isValid, "result is expected to be validd")
    result.map((_, res) => assertEquals(res, expected))
  }


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
  
  extension (s: String) {
    def tokenize = Lexer.tokenize(s)
  }

}
