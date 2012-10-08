package com.github.sakamotodesu.randomwalker
import org.specs2.mutable._
import com.github.sakamotodesu.randomwalker.RandomWalker._

class WalkerSpec extends Specification {
  "The 'Walker' after start" should {
    " be movable in (1,0)(0,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(0,0)),0))
      list must beEqualTo(List(Point(1,0),Point(0,1)))
    }
    " be movable in (1,2)(0,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(0,2)),0))
      list must beEqualTo(List(Point(1,2),Point(0,1)))
    }
    " be movable in (1,0)(2,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(2,0)),0))
      list must beEqualTo(List(Point(1,0),Point(2,1)))
    }
    " be movable in (1,2)(2,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(2,2)),0))
      list must beEqualTo(List(Point(1,2),Point(2,1)))
    }
    " be movable in (1,1)(0,0)(1,1) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(1,0)),0))
      list must beEqualTo(List(Point(2,0),Point(0,0),Point(1,1)))
    }
    " be movable in (2,1)(0,1)(1,0)(1,2) " in {
      val fool = new FoolWalker(Map(3,3),3)
      val list = fool.movable(new Way(List(Point(1,1)),0))
      list must beEqualTo(List(Point(2,1),Point(0,1),Point(1,0),Point(1,2)))
    }
  }

  "The 'Walker' walked a few steps" should {
    " be movable in (1,1)(0,2) " in {
      val fool = new FoolWalker(Map(3,3),5)
      val list = fool.movable(new Way(List(Point(0,1),Point(0,0)),0))
      list must beEqualTo(List(Point(1,1),Point(0,2)))
    }
    " be movable in (1,2) " in {
      val fool = new FoolWalker(Map(3,3),5)
      val list = fool.movable(new Way(List(Point(0,2),Point(0,1),Point(0,0)),0))
      list must beEqualTo(List(Point(1,2)))
    }
    " be movable in (2,0)(0,0) " in {
      val fool = new FoolWalker(Map(3,3),5)
      val list = fool.movable(new Way(List(Point(1,0),Point(1,1),Point(0,1),Point(0,0)),2))
      list must beEqualTo(List(Point(2,0),Point(0,0)))
    }
    " be movable in Nil " in {
      val fool = new FoolWalker(Map(3,3),5)
      val list = fool.movable(new Way(List(Point(0,0),Point(1,0),Point(1,1),Point(0,1),Point(0,0)),3))
      list must beEqualTo(Nil)
    }
  }
  "The 'Walker' close to the turn limit" should {
    " be movable in (1,1)(0,2) " in {
      val fool = new FoolWalker(Map(3,3),1)
      val list = fool.movable(new Way(List(Point(0,1),Point(0,0)),0))
      list must beEqualTo(List(Point(1,1),Point(0,2)))
    }
    " be movable in (2,1) " in {
      val fool = new FoolWalker(Map(3,3),1)
      val list = fool.movable(new Way(List(Point(1,1),Point(0,1),Point(0,0)),1))
      list must beEqualTo(List(Point(2,1)))
    }
    " be movable in Nil " in {
      val fool = new FoolWalker(Map(3,3),1)
      val list = fool.movable(new Way(List(Point(2,1),Point(1,1),Point(0,1),Point(0,0)),1))
      list must beEqualTo(Nil)
    }
  }
}
