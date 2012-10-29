package com.github.sakamotodesu.randomwalker

import scala.util.Random
import scala.annotation.tailrec
import com.github.sakamotodesu.randomwalker.way._
import com.github.sakamotodesu.randomwalker.walker._
import akka.actor._
import akka.routing.RoundRobinRouter

object RandomWalker {

  def main(args: Array[String]) {
    println("RandamWalker Start!")
    val system = ActorSystem("WalkingActorSystem")
    //val allWalkerMaster = system.actorOf(Props(new AllWalkerMaster(nrOfWorkers = 2, nrOfWalkPlans = 5, startPoint = Point(4,0), map = Map(8,8))), name = "walkMaster")
    val GAMaster = system.actorOf(Props(new GAMaster(nrOfWorkers = 2, nrOfMaxGenerations = 1000, startPoint = Point(4,0), map = Map(8,8), maxTurn = 15, worldRecord = 76)), name = "GAMaster")
    
    //allWalkerMaster ! StartWalking
    GAMaster ! StartWalking
  }

  sealed trait WalkingMessage
  case object StartWalking extends WalkingMessage
  case class LetsWalk(walker: Walker) extends WalkingMessage
  case class Result(walker: Walker, way: List[Point]) extends WalkingMessage
  case class StartNextGeneration(ways: List[List[Point]]) extends WalkingMessage
  case class ResultsNextGeneration(ways: List[List[Point]]) extends WalkingMessage

  class WalkActor extends Actor {
    def receive  = {
      case LetsWalk(walker) => sender ! Result(walker, walker.start)
    }
  }

  class GeneWalkActor (nrOfWorkers: Int ,map: Map, maxTurn: Int, GAMaster: ActorRef ) extends Actor {
    var nrOfResults: Int = _
    var nrOfWays: Int = _
    var resultWays: List[List[Point]] = List()

    val walkActorRouter = context.actorOf(Props[WalkActor].withRouter(RoundRobinRouter(nrOfWorkers)), name = "walkActorRouter")

    def receive = {
      case StartNextGeneration(ways) =>
        resultWays = List()
        nrOfResults = 0
	nrOfWays = ways.length
	ways.map( way => walkActorRouter ! LetsWalk(new ProbWalker(80, map, maxTurn, way) ) )

      case Result(walker, way) => 
        nrOfResults += 1
	resultWays = way::resultWays
        if (nrOfResults == nrOfWays) {
	  GAMaster ! ResultsNextGeneration(resultWays)
        }
    }
  }

  class GAMaster (nrOfWorkers: Int, nrOfMaxGenerations: Int, startPoint: Point, map: Map, maxTurn: Int, worldRecord: Int) extends Actor {
    var nrOfGenerations: Int = _
    var mostLongWay: List[Point] = List()
    val geneWalkActor = context.actorOf(Props( new GeneWalkActor( nrOfWorkers, map, maxTurn ,self ) ), name = "geneWalkActor")

    def receive = {
      case StartWalking => println("GAMaster start!"); nextGenerationStart( List( ( new NoPlanWalker( map, maxTurn, List( startPoint ) ) ).start ) )

      case ResultsNextGeneration(ways) => GA(ways)
    }

    def nextGenerationStart(ways: List[List[Point]]) = geneWalkActor ! StartNextGeneration(ways)
    def evaluate(ways: List[List[Point]]) = ways.maxBy(_.length)
    def makeNextGenerationSeed(way: List[Point]) = {
      @tailrec
      def separateByTurn(seedWay: List[Point], ways: List[List[Point]]):List[List[Point]] = {
        if ( seedWay.length <= 2 ) ways
	else if ( Way.isTurn( seedWay.head, seedWay.tail.tail.head ) == 1 ) separateByTurn(seedWay.tail, seedWay.tail.tail::ways)
	else separateByTurn(seedWay.tail, ways)
      }
      separateByTurn(way, List())
    }
    def GA(ways: List[List[Point]]) = {
      nrOfGenerations += 1
      val highscoreWay = evaluate(ways)
      println("Generation:" + nrOfGenerations)
      //printWay(new ProbWalker(80, map, maxTurn, highscoreWay), highscoreWay, map)
      if ( mostLongWay.length < highscoreWay.length ) mostLongWay = highscoreWay
      if ( highscoreWay.length >= worldRecord ) {
        println("Wow!!! new world record !!!!!!!!!!")
        printWay(new ProbWalker(80, map, maxTurn, highscoreWay), highscoreWay, map)
        context.system.shutdown()
        println("RandamWalker end!")
      }
      if ( nrOfGenerations == nrOfMaxGenerations ){
        println("max Generation!")
        printWay(new ProbWalker(80, map, maxTurn, mostLongWay), mostLongWay, map)
        context.system.shutdown()
        println("RandamWalker end!")
      }
      nextGenerationStart( makeNextGenerationSeed(highscoreWay ) )
    }
  }

  class AllWalkerMaster (nrOfWorkers: Int, nrOfWalkPlans: Int, startPoint: Point, map: Map)extends Actor {
    var nrOfResults: Int = _
    val walkActorRouter = context.actorOf(Props[WalkActor].withRouter(RoundRobinRouter(nrOfWorkers)), name = "walkActorRouter")

    def receive = {
      case StartWalking => 
        walkActorRouter ! LetsWalk(new NoPlanWalker(map, 15, List(Point(4,0))))
        walkActorRouter ! LetsWalk(new StraightWalker(map, 15, List(Point(4,0))))
        walkActorRouter ! LetsWalk(new StraightPrudentWalker(map, 15, List(Point(4,0))))
        walkActorRouter ! LetsWalk(new Plan8020Walker(map, 15, List(Point(4,0))))
        walkActorRouter ! LetsWalk(new ProbWalker(10, map, 15, List(Point(4,0))))

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
