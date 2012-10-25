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

  def walking(walker:Walker) = {
    val way = walker.start
    println("--------------------------------------------------------")
    println("walker:" + walker)
    println("steps:" + way.length)
    println("turn:" + walker.turnCount(way))
    println(way.reverse)
    println(walker.VisibleMap(way))
  }
}
