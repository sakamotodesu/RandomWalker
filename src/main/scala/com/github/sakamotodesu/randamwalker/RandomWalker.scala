package com.github.sakamotodesu.randomwalker
import scala.util.Random

object RandomWalker {
  def main(args: Array[String]) {
    println("Random Walk start!")
    val walker = new FoolWalker(Map(7,7),15)
    val way = walker.start(Point(4,0))
    println(way)
  }

  case class Map(x:Int,y:Int){
    def contains(p:Point) = if(0 <= p.x && p.x <= x && 0 <= p.y && p.y <= y) true else false
  }

  case class Point(x:Int,y:Int){
    def up = Point(x,y+1)
    def down = Point(x,y-1)
    def left = Point(x-1,y)
    def right = Point(x+1,y)
  }

  case class Way(way:List[Point], turn:Int){
    def add(p:Point, t:Int) =  Way(p::way, t)
    def stepCount = way.size - 1
    def now = way.head
    def before = way.tail.head
    def contains(p:Point) = way contains p
    def passed(p:Point) = ((way indexOfSlice List(p,now)) != -1) || ((way indexOfSlice List(now,p)) != -1)
    def isTurn(p:Point) = if(stepCount < 2) 0 else if((p.x - before.x ).abs == 2 || (p.y - before.x).abs == 2) 0 else 1 
    override def toString = "Step:" + stepCount + "\nWay:" + way.toString
  }

  abstract class Walker (map:Map,maxTurn:Int){
    def walk(w:Way, p:Point) =  w.add(p,w.turn + (w isTurn p))
    def movable(w:Way) = List(w.now.up, w.now.down, w.now.left, w.now.right).filter(n => map contains n).filter(n => !(w passed n)).filter(n => (w isTurn n) + w.turn <= maxTurn) 
    def think(w:Way,next:List[Point]):Point
    def start(p:Point) = {
      def walking(w:Way):Way = movable(w) match {
        case Nil => w
        case l => walking(walk(w,think(w,l)))
      }
      walking(Way(List(p),0))
    }
  }

  class FoolWalker(map:Map,maxTurn:Int) extends Walker(map,maxTurn){
    def think(w:Way,next:List[Point]) =  next(new Random() nextInt(next length))
  }
}
