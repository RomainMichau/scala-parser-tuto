//package com.rmichau.scalaparser.instances
//import com.rmichau.scalaparser.TestCommon.*
//import com.rmichau.scalaparser.instances.JSONParser.{JArray, JKey, JString}
//class SimpleDateParserTest extends munit.FunSuite {
//  test("key parser") {
//    val res=JSONParser.JKEY("\"key\":".tokenize, 0)
//    assertResult(res, JKey("key"))
//  }
//
//  test("value parser") {
//    val res = JSONParser.JSTRING("\"the value\"".tokenize, 0)
//    assertResult(res, JString("the value"))
//  }
//  test ("tes") {
//  }
//
////  test ("array") {
////    val res = JSONParser.JARRAY(
////      """
////        |["an","array","with elems"]
////        |""".stripMargin.tokenize, 0)
////    assertResult(res, JArray(List(JString("an"), JString("array"), JString("with elems")))
////    )
////  }
//}
