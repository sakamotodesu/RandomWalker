package com.github.sakamotodesu.randomwalker
import org.specs2.mutable._
import com.github.sakamotodesu.randomwalker.RandomWalker._

class WaySpec extends Specification {

 
  "The 'passed'" should {
    "return false" in {
      val way = List(Point(0,2),Point(0,1),Point(0,0))
      val walker = new NoPlanWalker(Map(3,3),3,Point(0,0))
     walker.passed(Point(0,3),way) must beFalse
    }
    "return true" in {
      val way = List(Point(0,1),Point(0,0))
      val walker = new NoPlanWalker(Map(3,3),3,Point(0,0))
      walker.passed (Point(0,0),way) must beTrue
    }
    "return true" in {
      val walker = new NoPlanWalker(Map(3,3),3,Point(0,0))
      val way = List(Point(0,0),Point(1,0),Point(1,1),Point(0,1),Point(0,0))
      walker.passed (Point(0,1),way) must beTrue
    }
  }

  "The 'isTurn'" should {
    "rutern 0" in {
      val walker = new NoPlanWalker(Map(3,3),3,Point(0,0))
      walker.isTurn(Point(0,2),Point(0,0)) must beEqualTo(0)
    }
    "rutern 0" in {
      val walker = new NoPlanWalker(Map(3,3),3,Point(0,0))
      walker.isTurn(Point(2,0),Point(0,0)) must beEqualTo(0)
    }
    "rutern 1" in {
      val walker = new NoPlanWalker(Map(3,3),3,Point(0,0))
      walker.isTurn(Point(1,1),Point(0,0)) must beEqualTo(1)
    }
  }
}
