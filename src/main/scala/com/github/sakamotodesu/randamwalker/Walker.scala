package com.github.sakamotodesu.randomwalker
import scala.util.Random


case class Map(x:Int,y:Int){
  def contains(p:Point) = if(0 <= p.x  && p.x <= x - 1 && 0 <= p.y && p.y <= y - 1) true else false
  def VisibleMap(way:List[Point]) =  {
    def expand(n:Int) = 2 * n + 1
    def path(n:Int) = expand(n)/2
    val vm = Array.ofDim[String](expand(x), expand(y))
    for ( i <- 0 to expand(x) - 1; j <- 0 to expand(y) - 1) vm(i)(j) = " " //TODO: do not use for
    for ( i <- 0 to x - 1; j <- 0 to y - 1) vm(expand(i))(expand(j)) = "p" //TODO: do not use for
    vm(expand(way.head.x))(expand(way.head.y)) = "E"
    vm(expand(way.last.x))(expand(way.last.y)) = "S"
    way zip way.tail map( n => vm(path(n._1.x)+path(n._2.x)+1)(path(n._1.y)+path(n._2.y)+1) = "+")
    vm.map(_.fold("")((z,n)=> z + " " + n + " ")).fold("")((z,n)=>z+n+"\n") 
  }
}

case class Point(x:Int,y:Int) {
  def up = Point(x+1,y)
  def down = Point(x-1,y)
  def left = Point(x,y-1)
  def right = Point(x,y+1)
}

abstract class Walker (map:Map,maxTurn:Int){
  def passed(p:Point,way:List[Point]) = ((way indexOfSlice List(p,way.head)) != -1) || ((way indexOfSlice List(way.head,p)) != -1)
  def isTurn(next:Point,prev:Point) =  if((next.x - prev.x ).abs == 2 || (next.y - prev.y).abs == 2) 0 else 1 
  def turnCount(way:List[Point]) = if(way.length < 2 ) 0 else ( way zip  way.tail.tail).map( n => isTurn(n._1, n._2)).sum
  def walk(w:List[Point], p:Point) =  p::w
  def movable(w:List[Point]) = List(w.head.up, w.head.down, w.head.left, w.head.right).filter(map contains _).filter( n => !(passed(n,w))).filter( n =>  if (w.length < 2 ) true else (isTurn(n,w.tail.head) + turnCount(w) <= maxTurn)) 
  def think(w:List[Point],next:List[Point]):Point
  def start(p:Point) = {
    def walking(w:List[Point]):List[Point] = movable(w) match {
      case Nil => w
      case l => walking(walk(w,think(w,l)))
    }
    walking(List(p))
  }
}

class NoPlanWalker(map:Map,maxTurn:Int) extends Walker(map,maxTurn){
  def think(w:List[Point],next:List[Point]) =  next(new Random() nextInt(next length))
  override def toString = "NoPlanWalker"
}

class StraightWalker(map:Map,maxTurn:Int) extends Walker(map,maxTurn){
  def think(w:List[Point],next:List[Point]) = {
   next filter( n => if( w.length < 2 ) true else (isTurn(n,w.tail.head) == 0 ))  match {
      case List(p:Point) => p
      case _ => next(new Random() nextInt(next length))
    }
  }
  override def toString = "StraightWalker"
}

class StraightPrudent(map:Map,maxTurn:Int) extends Walker(map,maxTurn){
  def think(w:List[Point],next:List[Point]) = {
    def check(p:Point) = {
      movable(walk(w,p)) match {
 case Nil =>  false
        case _ => true
      }
    }
    val nn = next filter( n => check(n)) 
    if (nn.length == 0) next(new Random() nextInt(next length))
    else nn filter( n => if( w.length < 2 ) true else (isTurn(n,w.tail.head) == 0 ))  match {
           case List(p:Point) => p
           case _ => nn(new Random() nextInt(nn length))
         }
  }   
  override def toString = "StraightPrudentWalker"
}

