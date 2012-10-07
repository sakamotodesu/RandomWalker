package com.github.sakamotodesu.randomwalker
import org.specs2.mutable._
import com.github.sakamotodesu.randomwalker.RandomWalker._

class WaySpec extends Specification {

  "The 'Way.stepCount'" should {
    "be 0 if start" in {
      val way = Way(List(Point(0,0)),0)
      way.stepCount must beEqualTo(0)
    }
    "be 1 if 1 step" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      way1.stepCount must beEqualTo(1)
    }
    "be 5 if 5 steps" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      val way2 = way1.add(Point(0,2),0)
      val way3 = way2.add(Point(0,3),0)
      val way4 = way3.add(Point(0,4),0)
      val way5 = way4.add(Point(0,5),0)
      way5.stepCount must beEqualTo(5)
    }
  }
 
  "The 'Way.passed'" should {
    "return false" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      val way2 = way1.add(Point(0,2),0)
      (way2 passed Point(0,3)) must beFalse
    }
    "return true" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      (way1 passed Point(0,0)) must beTrue
    }
    "return true" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      val way2 = way1.add(Point(1,1),0)
      val way3 = way2.add(Point(1,0),0)
      val way4 = way3.add(Point(0,0),0)
      (way4 passed Point(0,1)) must beTrue
    }
    "return true" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      val way2 = way1.add(Point(1,1),0)
      val way3 = way2.add(Point(1,0),0)
      val way4 = way3.add(Point(0,0),0)
      (way4 passed Point(1,0)) must beTrue
    }
    "return false" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      val way2 = way1.add(Point(1,1),0)
      val way3 = way2.add(Point(1,0),0)
      val way4 = way3.add(Point(0,0),0)
      val way5 = way4.add(Point(0,1),0)
      (way5 passed Point(0,2)) must beFalse
    }
  }
  "The 'Way.isTurn'" should {
    "rutern 0" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      (way1 isTurn Point(0,2)) must beEqualTo(0)
    }
    "rutern 0" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(1,0),0)
      (way1 isTurn Point(2,0)) must beEqualTo(0)
    }
    "rutern 0" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(0,1),0)
      (way1 isTurn Point(1,1)) must beEqualTo(0)
    }
    "rutern 0" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(1,0),0)
      (way1 isTurn Point(1,1)) must beEqualTo(0)
    }
    "rutern 1" in {
      val way = Way(List(Point(0,0)),0)
      val way1 = way.add(Point(1,0),0)
      val way2 = way1.add(Point(2,0),0)
      (way2 isTurn Point(2,1)) must beEqualTo(1)
    }
  }
}
