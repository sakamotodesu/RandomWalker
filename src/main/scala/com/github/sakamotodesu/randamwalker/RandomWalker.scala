package com.github.sakamotodesu.randomwalker
import scala.util.Random
import com.github.sakamotodesu.randomwalker._
import akka.actor._

object RandomWalker {
  def main(args: Array[String]) {
    println("Random Walk start!")
    val map = Map(8,8)
    val system = ActorSystem("WalkingActorSystem")
    val walkActor = system.actorOf(Props[WalkActor], name = "walkActor")
    
    walkActor ! LetsWalk(new NoPlanWalker(map, 15, Point(4,0)))
    walkActor ! LetsWalk(new StraightWalker(map, 15, Point(4,0)))
    walkActor ! LetsWalk(new StraightPrudentWalker(map, 15, Point(4,0)))
    walkActor ! LetsWalk(new Plan8020Walker(map, 15, Point(4,0)))
    walkActor ! LetsWalk(new ProbWalker(10, map, 15, Point(4,0)))
   system.shutdown()
  }

  case class LetsWalk(walker:Walker)

  class WalkActor extends Actor {
    def receive  = {
      case LetsWalk(walker) => walking(walker)
                               context.stop(self)
    }
  }
  def VisibleMap(way:List[Point],map:Map) =  {
    def expand(n:Int) = 2 * n + 1
    def path(n:Int) = expand(n)/2
    val vm = Array.ofDim[String](expand(map.x), expand(map.y))
    for ( i <- 0 to expand(map.x) - 1; j <- 0 to expand(map.y) - 1) vm(i)(j) = " " //TODO: do not use for
    for ( i <- 0 to map.x - 1; j <- 0 to map.y - 1) vm(expand(i))(expand(j)) = "p" //TODO: do not use for
    vm(expand(way.head.x))(expand(way.head.y)) = "E"
    vm(expand(way.last.x))(expand(way.last.y)) = "S"
    way zip way.tail map( n => vm(path(n._1.x)+path(n._2.x)+1)(path(n._1.y)+path(n._2.y)+1) = "+")
    vm.map(_.fold("")((z,n)=> z + " " + n + " ")).fold("")((z,n)=>z+n+"\n") 
  }

  def walking(walker:Walker) = {
    val way = walker.start
    println("--------------------------------------------------------")
    println("walker:" + walker)
    println("steps:" + way.length)
    println("turn:" + walker.turnCount(way))
    println(way.reverse)
    println(VisibleMap(way,walker.map))
  }
}
