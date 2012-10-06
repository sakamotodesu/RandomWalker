package com.github.sakamotodesu.randomwalker
import scala.util.Random

object RandomWalker {
  def main(args: Array[String]) {
    println("Random Walk start!")
    val map = Map(3,3)
    val walker = new FoolWalker(map,15)
    val way = walker.start(Point(0,0))
    println(way)
    println(VisibleMap(map,way))
  }

  case class Map(x:Int,y:Int){
    def contains(p:Point) = if(0 <= p.x && p.x <= x && 0 <= p.y && p.y <= y) true else false
  }

  def VisibleMap(map:Map,way:Way) =  {
    def expand(n:Int) = 2 * n + 1
    val vm = Array.ofDim[String](expand(map.x), expand(map.y))
    for ( i <- 0 to expand(map.x) - 1; j <- 0 to expand(map.y) - 1) vm(i)(j) = "a" //TODO: do not use for
    for ( i <- 0 to map.x - 1; j <- 0 to map.y - 1) vm(expand(i))(expand(j)) = "p" //TODO: do not use for
    vm(expand(way.head.x))(expand(way.head.y)) = "E"
    vm(expand(way.last.x))(expand(way.last.y)) = "S"
    vm.map(_.fold("")((z,n)=> z + " " + n + " ")).fold("")((z,n)=>z+n+"\n") 
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
    def head = way.head
    def last = way.last
    def before = way.tail.head // TODO: safe
    def contains(p:Point) = way contains p
    def passed(p:Point) = ((way indexOfSlice List(p,head)) != -1) || ((way indexOfSlice List(head,p)) != -1)
    def isTurn(p:Point) = if(stepCount < 2) 0 else if((p.x - before.x ).abs == 2 || (p.y - before.x).abs == 2) 0 else 1 
    def path = if (stepCount < 2 ) (head,Nil) else (head,before)
    override def toString = "Step:" + stepCount + "\nWay:" + way.toString
  }

  abstract class Walker (map:Map,maxTurn:Int){
    def walk(w:Way, p:Point) =  w.add(p,w.turn + (w isTurn p))
    // TODO: sometime over map?
    def movable(w:Way) = List(w.head.up, w.head.down, w.head.left, w.head.right).filter(map contains _).filter( n => !(w passed n)).filter( n => (w isTurn n) + w.turn <= maxTurn) 
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
