package com.rmichau.scalaparser.instances
import com.rmichau.scalaparser.TestCommon.*
import com.rmichau.scalaparser.instances.JSONParser.{JKey, JString}
class JSONParserTests extends munit.FunSuite {
  test("key parser") {
    val res=JSONParser.JKEY("\"key\":".tokenize, 0)
    assertResult(res, JKey("key"))
  }

  test("value parser") {
    val res = JSONParser.JSTRING("\"the value\"".tokenize, 0)
    assertResult(res, JString("the value"))
  }
  test ("tes") {
    val res = JSONParser.teststuff("\"bro\":]".tokenize, 0)
    println(res)
  }
//
//  test ("array") {
//    val res = JSONParser.JARRAY(
//      """
//        |["an","array"]
//        |""".stripMargin.tokenize, 0)
//    println(res)
//  }
}
