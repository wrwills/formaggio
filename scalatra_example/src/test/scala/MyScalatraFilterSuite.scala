package com.example

import org.scalatra._
import org.scalatra.test.scalatest._
import org.scalatest.matchers._

class MyScalatraFilterSuite extends ScalatraFunSuite with ShouldMatchers {
  addFilter(classOf[MyScalatraFilter], "/*")

  test("GET / returns status 200") {
    get("/") { 
      status should equal (200)
    }
  }
}
