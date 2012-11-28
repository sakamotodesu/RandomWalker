package com.github.sakamotodesu.randomwalker.walker

import com.github.sakamotodesu.randomwalker.way._
import scala.util.Random
import scala.annotation.tailrec

abstract case class Walker(map: Map, maxTurn: Int, way: List[Point]) {
 
  def walk(w: List[Point], p: Point) =  p :: w
 
  def inMap(p: Point) = map contains p

  def notPassed(p: Point, w: List[Point]) = ! Way.passed(p, w)

  def withinLimit(p: Point, w: List[Point]) = if (w.length < 2 ) true else (Way.isTurn(p, w.tail.head) + Way.turnCount(w) <= maxTurn)

  def movable(w: List[Point]) = {
    List(w.head.up, w.head.down, w.head.left, w.head.right) filter {inMap(_)} filter {notPassed(_, w)} filter {withinLimit(_, w)} 
  }
 
  def think(w: List[Point], next: List[Point]): Point
 
  def start = {
    @tailrec
    def walking(w: List[Point]): List[Point] = movable(w) match {
      case Nil => w
      case l => walking(walk(w, think(w, l)))
    }
    walking(way)
  }

}

trait RandomWalk { 
 
  def random(w: List[Point], next: List[Point]) =  next(new Random() nextInt(next length))

}

trait StraightWalk {

  def straight(w: List[Point], next: List[Point]) = {
   next filter( n => if ( w.length < 2 ) true else (Way.isTurn(n, w.tail.head) == 0 ))  match {
      case List(p: Point) => p
      case _ => next(new Random() nextInt(next length))
    }
  }

}

trait Probability {

  def toBeTrue(n: Int) = (new Random() nextInt(100)) < n

  def toBeFalse(n: Int) = (new Random() nextInt(100)) >= n

}

class NoPlanWalker(
    map: Map,
    maxTurn: Int,
    way: List[Point])
  extends Walker(map, maxTurn, way)
  with RandomWalk {

  def think(w: List[Point], next: List[Point]) =  random(w, next)

  override def toString = "NoPlanWalker"

}

class StraightWalker(
    map: Map,
    maxTurn: Int,
    way: List[Point])
  extends Walker(map, maxTurn, way)
  with StraightWalk {
  
  def think(w: List[Point], next: List[Point]) = straight(w, next)
  
  override def toString = "StraightWalker"

}

class StraightPrudentWalker(
    map: Map,
    maxTurn: Int,
    way: List[Point])
  extends Walker(map, maxTurn, way)
  with RandomWalk
  with StraightWalk {

  override def think(w: List[Point], next: List[Point]) = {
    def check(p: Point) = {
      movable(walk(w, p)) match {
        case Nil =>  false
        case _ => true
      }
    }
    next filter( n => check(n)) match {
      case Nil => random(w, next)
      case nn => straight(w, nn) 
    } 
  } 

  override def toString = "StraightPrudentWalker"

}

class Plan8020Walker(
    map: Map,
    maxTurn: Int,
    way: List[Point])
  extends Walker(map, maxTurn, way)
  with RandomWalk
  with StraightWalk
  with Probability {

  override def think(w: List[Point], next: List[Point]) =
    if (toBeTrue(80)) straight(w, next) else random(w, next)

  override def toString = "Plan8020Walker"

}

class ProbWalker(
    prob: Int,
    map: Map,
    maxTurn: Int,
    way: List[Point])
  extends Walker(map, maxTurn, way)
  with RandomWalk
  with StraightWalk
  with Probability {

  override def think(w: List[Point], next: List[Point]) =
    if (toBeTrue(prob)) straight(w, next) else random(w, next)

  override def toString = "ProbWalker(" + prob + "%)"

}
