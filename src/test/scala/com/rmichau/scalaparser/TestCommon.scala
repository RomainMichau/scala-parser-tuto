package com.rmichau.scalaparser

import cats.data.Validated.{Invalid, Valid}
import munit.Assertions.{assertEquals, fail}

object TestCommon {

  extension [R](p: ParseResultSt[R]) {
    def assertSuccess(r: R): Unit = p match {
      case Valid(_, res) => assertEquals(res, r)
      case _             => fail("expect valid result")
    }

    def assertFailure(): Unit = assert(p.isInvalid, "expect invalid ParseResult")

    def assertFailure(r: String): Unit = p match {
      case Invalid(e) => assertEquals(e.head, r)
      case _          => fail("expect invalid result")
    }

    def assertSuccess(): Unit = p match {
      case Invalid(e) => fail(s"expect valid ParseResult. Got error $e")
      case _          =>
    }
  }

}
