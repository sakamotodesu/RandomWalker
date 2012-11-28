package com.github.sakamotodesu.randomwalker

import org.specs2.mutable._
import com.github.sakamotodesu.randomwalker.way._
import com.github.sakamotodesu.randomwalker.walker._

class WalkerSpec extends Specification {
  "The 'Walker' after start" should {
    " be movable in (1, 0)(0, 1) " in {
      val walker = new NoPlanWalker(Map(3, 3), 3, List(Point(0, 0)))
      val list = walker.movable(List(Point(0, 0)))
      list must beEqualTo(List(Point(1, 0), Point(0, 1)))
    }
    " be movable in (1, 2)(0, 1) " in {
      val walker = new NoPlanWalker(Map(3, 3), 3, List(Point(0, 2)))
      val list = walker.movable(List(Point(0, 2)))
      list must beEqualTo(List(Point(1, 2), Point(0, 1)))
    }
    " be movable in (1, 0)(2, 1) " in {
      val walker = new NoPlanWalker(Map(3, 3), 3, List(Point(2, 0)))
      val list = walker.movable(List(Point(2, 0)))
      list must beEqualTo(List(Point(1, 0), Point(2, 1)))
    }
    " be movable in (1, 2)(2, 1) " in {
      val walker = new NoPlanWalker(Map(3, 3), 3, List(Point(2, 2)))
      val list = walker.movable(List(Point(2, 2)))
      list must beEqualTo(List(Point(1, 2), Point(2, 1)))
    }
    " be movable in (1, 1)(0, 0)(1, 1) " in {
      val walker = new NoPlanWalker(Map(3, 3), 3, List(Point(1, 0)))
      val list = walker.movable(List(Point(1, 0)))
      list must beEqualTo(List(Point(2, 0), Point(0, 0), Point(1, 1)))
    }
    " be movable in (2, 1)(0, 1)(1, 0)(1, 2) " in {
      val walker = new NoPlanWalker(Map(3, 3), 3, List(Point(1, 1)))
      val list = walker.movable(List(Point(1, 1)))
      list must beEqualTo(List(Point(2, 1), Point(0, 1), Point(1, 0), Point(1, 2)))
    }
  }

  "The 'Walker' walked a few steps" should {
    " be movable in (1, 1)(0, 2) " in {
      val walker = new NoPlanWalker(Map(3, 3), 5, List(Point(0, 0)))
      val list = walker.movable(List(Point(0, 1), Point(0, 0)))
      list must beEqualTo(List(Point(1, 1), Point(0, 2)))
    }
    " be movable in (1, 2) " in {
      val walker = new NoPlanWalker(Map(3, 3), 5, List(Point(0, 0)))
      val list = walker.movable(List(Point(0, 2), Point(0, 1), Point(0, 0)))
      list must beEqualTo(List(Point(1, 2)))
    }
    " be movable in (2, 0)(0, 0) " in {
      val walker = new NoPlanWalker(Map(3, 3), 5, List(Point(0, 0)))
      val list = walker.movable(List(Point(1, 0), Point(1, 1), Point(0, 1), Point(0, 0)))
      list must beEqualTo(List(Point(2, 0), Point(0, 0)))
    }
    " be movable in Nil " in {
      val walker = new NoPlanWalker(Map(3, 3), 5, List(Point(0, 0)))
      val list = walker.movable(List(Point(0, 0), Point(1, 0), Point(1, 1), Point(0, 1), Point(0, 0)))
      list must beEqualTo(Nil)
    }
  }

  "The 'Walker' close to the turn limit" should {
    " be movable in (1, 1)(0, 2) " in {
      val walker = new NoPlanWalker(Map(3, 3), 1, List(Point(0, 0)))
      val list = walker.movable(List(Point(0, 1), Point(0, 0)))
      list must beEqualTo(List(Point(1, 1), Point(0, 2)))
    }
    " be movable in (2, 1) " in {
      val walker = new NoPlanWalker(Map(3, 3), 1, List(Point(0, 0)))
      val list = walker.movable(List(Point(1, 1), Point(0, 1), Point(0, 0)))
      list must beEqualTo(List(Point(2, 1)))
    }
    " be movable in Nil " in {
      val walker = new NoPlanWalker(Map(3, 3), 1, List(Point(0, 0)))
      val list = walker.movable(List(Point(2, 1), Point(1, 1), Point(0, 1), Point(0, 0)))
      list must beEqualTo(Nil)
    }
  }
}
