package com.github.sakamotodesu.randomwalker
import org.specs2.mutable._
import com.github.sakamotodesu.randomwalker.RandomWalker._

class WalkerSpec extends Specification {
  "The 'Walker'" should {
    " can move to (1,0)(0,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(0,0)),0))
      list must beEqualTo(List(Point(1,0),Point(0,1)))
    }
    " can move to (1,2)(0,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(0,2)),0))
      list must beEqualTo(List(Point(1,2),Point(0,1)))
    }
    " can move to (1,0)(2,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(2,0)),0))
      list must beEqualTo(List(Point(1,0),Point(2,1)))
    }
    " can move to (1,2)(2,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(2,2)),0))
      list must beEqualTo(List(Point(1,2),Point(2,1)))
    }
    " can move to (1,1)(0,0)(1,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(1,0)),0))
      list must beEqualTo(List(Point(2,0),Point(0,0),Point(1,1)))
    }
    " can move to (2,1)(0,1)(1,0)(1,2) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(1,1)),0))
      list must beEqualTo(List(Point(2,1),Point(0,1),Point(1,0),Point(1,2)))
    }
  }
}
