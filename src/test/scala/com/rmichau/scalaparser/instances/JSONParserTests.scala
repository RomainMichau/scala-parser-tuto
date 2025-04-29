package com.rmichau.scalaparser.instances
import JSONParser.*
import com.rmichau.scalaparser.Lexer
class JSONParserTests extends munit.FunSuite {
  test("basic lexer") {
    val res=JSONParser.KEY(Lexer.tokenize("\"key\":"), 0)
    println(res)
  }
}
