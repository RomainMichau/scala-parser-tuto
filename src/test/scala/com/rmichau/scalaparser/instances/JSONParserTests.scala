package com.rmichau.scalaparser.instances
import com.rmichau.scalaparser.TestCommon.*
import com.rmichau.scalaparser.instances.JSONParser.JSONParser

class JSONParserTests extends munit.FunSuite {

  val input = """
                |{
                |  "name": "Alice",
                |  "status": "active",
                |  "tags": ["admin", "editor", "reviewer"],
                |  "profile": {
                |    "firstName": "Alice",
                |    "lastName": "Smith",
                |    "location": "Wonderland"
                |  }
                |}
                |""".stripMargin.filterNot(_.isWhitespace)
  test("key parser") {
    JSONParser(input).assertSuccess()
  }
}
