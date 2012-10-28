package com.github.sakamotodesu.randomwalker
import scala.util.Random
import com.github.sakamotodesu.randomwalker.way._
import com.github.sakamotodesu.randomwalker.walker._
import akka.actor._
import akka.routing.RoundRobinRouter

object RandomWalker {
  def main(args: Array[String]) {
    println("Random Walk start!")
    val system = ActorSystem("WalkingActorSystem")
    val walkMaster = system.actorOf(Props(new WalkMaster(nrOfWorkers = 2, nrOfWalkPlans = 5, startPoint = Point(4,0), map = Map(8,8))), name = "walkMaster")
    
    walkMaster ! StartWalking
  }

  sealed trait WalkingMessage
  case object StartWalking extends WalkingMessage
  case class LetsWalk(walker: Walker) extends WalkingMessage
  case class Result(walker: Walker, way: List[Point]) extends WalkingMessage

  class WalkActor extends Actor {
    def receive  = {
      case LetsWalk(walker) => sender ! Result(walker, walker.start)
    }
  }

  class WalkMaster (nrOfWorkers: Int, nrOfWalkPlans: Int, startPoint: Point, map: Map)extends Actor {
    var nrOfResults: Int = _
    val walkActorRouter = context.actorOf(Props[WalkActor].withRouter(RoundRobinRouter(nrOfWorkers)), name = "walkActorRouter")

    def receive = {
      case StartWalking => 
        walkActorRouter ! LetsWalk(new NoPlanWalker(map, 15, Point(4,0)))
        walkActorRouter ! LetsWalk(new StraightWalker(map, 15, Point(4,0)))
        walkActorRouter ! LetsWalk(new StraightPrudentWalker(map, 15, Point(4,0)))
        walkActorRouter ! LetsWalk(new Plan8020Walker(map, 15, Point(4,0)))
        walkActorRouter ! LetsWalk(new ProbWalker(10, map, 15, Point(4,0)))

      case Result(walker, way) => 
        nrOfResults += 1
        printWay(walker, way, map)
        if (nrOfResults == nrOfWalkPlans) {
          context.stop(self)
          context.system.shutdown()
        }
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


  def printWay(walker: Walker, way: List[Point], map: Map) = {
    println("--------------------------------------------------------")
    println("walker:" + walker.toString)
    println("steps:" + way.length)
    println("turn:" + Way.turnCount(way))
    println(way.reverse)
    println(VisibleMap(way, map))
  }
}
