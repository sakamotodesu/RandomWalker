package com.github.sakamotodesu.randomwalker

import org.specs2.mutable._
import com.github.sakamotodesu.randomwalker.way._

class WaySpec extends Specification {

 
  "The 'passed'" should {
    "return false" in {
      val way = List(Point(0, 2), Point(0, 1), Point(0, 0))
      Way.passed(Point(0, 3), way) must beFalse
    }
    "return true" in {
      val way = List(Point(0, 1), Point(0, 0))
      Way.passed (Point(0, 0), way) must beTrue
    }
    "return true" in {
      val way = List(Point(0, 0), Point(1, 0), Point(1, 1), Point(0, 1), Point(0, 0))
      Way.passed (Point(0, 1), way) must beTrue
    }
  }

  "The 'isTurn'" should {
    "rutern 0" in {
      Way.isTurn(Point(0, 2), Point(0, 0)) must beEqualTo(0)
    }
    "rutern 0" in {
      Way.isTurn(Point(2, 0), Point(0, 0)) must beEqualTo(0)
    }
    "rutern 1" in {
      Way.isTurn(Point(1, 1), Point(0, 0)) must beEqualTo(1)
    }
  }

  "The 'lessThan2'" should {
    "return true" in {
      Way.lessThan2(List()) must beTrue
    }
    "return true" in {
      Way.lessThan2(List(Point(0, 0))) must beTrue
    }
    "return false" in {
      Way.lessThan2(List(Point(1, 0), Point(0, 0))) must beFalse
    }
    "return false" in {
      Way.lessThan2(List(Point(1, 1), Point(1, 0), Point(0, 0))) must beFalse
    }
  }
}
