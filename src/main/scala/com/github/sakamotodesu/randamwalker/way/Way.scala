package com.github.sakamotodesu.randomwalker.way

import scala.util.Random
import scala.annotation.tailrec

object Way {
  case class Point(x: Int, y: Int) {
  
    def up = Point(x + 1, y)
  
    def down = Point(x - 1, y)
  
    def left = Point(x, y - 1)
  
    def right = Point(x, y + 1)
  
  }
  
  case class Grid(x: Int, y: Int) {
  
    def contains(p: Point) = if (0 <= p.x  && p.x <= x - 1 && 0 <= p.y && p.y <= y - 1) true else false
  
  }
  
  
  def passed(p: Point, way: List[Point]) = ((way indexOfSlice List(p, way.head)) != -1) || ((way indexOfSlice List(way.head, p)) != -1)

  def isTurn(next: Point, prev: Point) =  if ((next.x - prev.x ).abs == 2 || (next.y - prev.y).abs == 2) 0 else 1 

  def turnCount(way: List[Point]) = if (way.length < 2 ) 0 else ( way zip  way.tail.tail).map( n => isTurn(n._1, n._2)).sum

  def lessThan2(way: List[Point]) = way match {

    case a :: b :: c => false

    case _ => true

  }

}
