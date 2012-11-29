package com.github.sakamotodesu.randomwalker.walker

import com.github.sakamotodesu.randomwalker.way.Way._
import scala.util.Random
import scala.annotation.tailrec

object Walker {

  /**
   * The Walker class will receive a walking plan, walk in accordance with it. 
   */
  case class Walker(grid: Grid, maxTurn: Int, way: List[Point], plan: ((List[Point], List[Point]) => Point)) {

    def inGrid(p: Point) = grid contains p

    def notPassed(p: Point, w: List[Point]) = ! passed(p, w)

    def withinLimit(p: Point, w: List[Point]) = if (lessThan2(w)) true else (isTurn(p, w.tail.head) + turnCount(w) <= maxTurn)

    def movable(w: List[Point]) =
      List(w.head.up, w.head.down, w.head.left, w.head.right) filter (inGrid(_)) filter (notPassed(_, w)) filter (withinLimit(_, w))

    def think(w: List[Point], next: List[Point]): Point = plan(w, next)

    def start = {
      @tailrec
      def walking(w: List[Point]): List[Point] = movable(w) match {
        case Nil => w
        case l => walking(think(w, l) :: w)
      }
      walking(way)
    }

  }
  
  def random(w: List[Point], next: List[Point]) =  next(new Random() nextInt(next length))

  def toBeTrue(n: Int) = (new Random() nextInt(100)) < n
  
  def toBeFalse(n: Int) = (new Random() nextInt(100)) >= n

  def straight(w: List[Point], next: List[Point]) = {
   next filter( n => if (lessThan2(w)) true else (isTurn(n, w.tail.head) == 0 ))  match {

      case List(p: Point) => p

      case _ => next(new Random() nextInt(next length))
  
    }
  }

  def prob(w: List[Point], next: List[Point], prob: Int) =
      if (toBeTrue(prob)) straight(w, next) else random(w, next)

  /**
   * This is a plan to proceed without thinking.
   */
  def noPlan(w: List[Point], next: List[Point]) =  random(w, next)

  /**
   * This plan is straight with a probability that is specified, go to random otherwise.
   */
  def probPlan(w: List[Point], next: List[Point]) =  prob(w, next, 80)
}
