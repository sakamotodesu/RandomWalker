package com.github.sakamotodesu.randomwalker
import scala.util.Random
import com.github.sakamotodesu.randomwalker._

object RandomWalker {
  def main(args: Array[String]) {
    println("Random Walk start!")
    val map = Map(8,8)
    walking(new NoPlanWalker(map,15),map,Point(4,0))
    walking(new StraightWalker(map,15),map,Point(4,0))
    walking(new StraightPrudent(map,15),map,Point(4,0))
  }

  def walking(walker:Walker,map:Map,start:Point) = {
    val way = walker.start(start)
    println("--------------------------------------------------------")
    println("walker:" + walker)
    println("steps:" + way.length)
    println("turn:" + walker.turnCount(way))
    println(way.reverse)
    println(map.VisibleMap(way))
  }
}
