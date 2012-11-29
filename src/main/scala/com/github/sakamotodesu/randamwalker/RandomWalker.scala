package com.github.sakamotodesu.randomwalker

import scala.util.Random
import scala.math
import scala.annotation.tailrec
import com.github.sakamotodesu.randomwalker.way.Way._
import com.github.sakamotodesu.randomwalker.walker.Walker._
import akka.actor._
import akka.routing.RoundRobinRouter
import akka.event.Logging


object RandomWalker {

  def main(args: Array[String]) {
    val system = ActorSystem("WalkingActorSystem")
    val GAMaster = 
      system.actorOf(Props(new GAMaster(
                                 nrOfWorkers = 2,
                                 nrOfMaxGenerations = 100,
                                 startPoint = Point(4, 0),
                                 grid = Grid(8, 8),
                                 maxTurn = 15,
                                 worldRecord = 76)
                               ), name = "GAMaster")

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

  class GeneWalkActor(
      nrOfWorkers: Int,
      grid: Grid,
      maxTurn: Int,
      GAMaster: ActorRef)
    extends Actor {
    var nrOfResults: Int = _
    var nrOfWays: Int = _
    var resultWays: List[List[Point]] = List()

    val walkActorRouter =
      context.actorOf(Props[WalkActor].withRouter(RoundRobinRouter(nrOfWorkers)), name = "walkActorRouter")

    def receive = {
      case StartNextGeneration(ways) => {
        resultWays = List()
        nrOfResults = 0
        nrOfWays = ways.length
        ways.map(way => walkActorRouter ! LetsWalk(new Walker(grid, maxTurn, way)(probPlan _)))
      }

      case Result(walker, way) => {
        nrOfResults += 1
        resultWays = way :: resultWays
        if (nrOfResults == nrOfWays) {
          GAMaster ! ResultsNextGeneration(resultWays)
        }
      }
    }
  }

  /**
   * The GAMaster class is a class that manages a genetic algorithm.
   *
   * In this genetic algorithm decomposes the Way that you selected from the previous generation first.
   * We will make them as a material then walking again.
   * 
   * TODO:
   * I should improve the algorithm more better.
   * First of all, what is the genetic algorithm is suitable to this problem?
   */
  class GAMaster(
      nrOfWorkers: Int,
      nrOfMaxGenerations: Int,
      startPoint: Point,
      grid: Grid,
      maxTurn: Int,
      worldRecord: Int)
    extends Actor {
    val log = Logging(context.system, this)
    log.info("RandamWalker Start!")
    var nrOfGenerations: Int = _
    var highscoreWay: List[Point] = List()
    val geneWalkActor =
      context.actorOf(Props(new GeneWalkActor(nrOfWorkers, grid, maxTurn, self)), name = "geneWalkActor")

    def receive = {
      case StartWalking => nextGenerationStart(List(new Walker(grid, maxTurn, List(startPoint))(noPlan _).start))

      case ResultsNextGeneration(ways) => GA(ways)
    }

    def nextGenerationStart(ways: List[List[Point]]) = geneWalkActor ! StartNextGeneration(ways)

    def lottery(len: Int) = (math.pow(new Random() nextInt len, 3) / math.pow(len, 2)) toInt

    def evaluate(ways: List[List[Point]]) = {
      log.debug((ways.map(_.length).sum) / ways.length toString)
      ways.sortBy(s => s.length).reverse(lottery(ways.length))
    }

    def makeNextGenerationSeed(way: List[Point]) = {
      @tailrec
      def separateByTurn(seedWay: List[Point], ways: List[List[Point]]): List[List[Point]] = {
        if (seedWay.length <= 2) ways
        else if (isTurn(seedWay.head, seedWay.tail.tail.head) == 1) separateByTurn(seedWay.tail, seedWay.tail.tail::ways)
        else separateByTurn(seedWay.tail, ways)
      }
      val nextWays = separateByTurn(way, List())
      nextWays ::: nextWays ::: nextWays ::: nextWays ::: nextWays
    }

    def GA(ways: List[List[Point]]) = {
      nrOfGenerations += 1
      val eliteWay = evaluate(ways)

      if (highscoreWay.length < eliteWay.length) {
        log.info("update highscore!")
        printResult(eliteWay, grid, nrOfGenerations)
        highscoreWay = eliteWay
      }

      if (eliteWay.length - 1 > worldRecord) {
        log.info("Wow!!! new world record !!!!!!!!!!")
        printResult(eliteWay, grid, nrOfGenerations)
        context.system.shutdown()
        log.info("RandamWalker end!")
      } else if (nrOfGenerations == nrOfMaxGenerations){
        log.info("max Generation!")
        printResult(highscoreWay, grid, nrOfGenerations)
        context.system.shutdown()
        log.info("RandamWalker end!")
      } else {
        nextGenerationStart(makeNextGenerationSeed(eliteWay))
      }

    }

    def expand(n: Int) = 2 * n + 1

    def path(n: Int) = expand(n) / 2

    def VisibleGrid(way: List[Point], grid: Grid) =  {

      val vm = Array.ofDim[String](expand(grid.x), expand(grid.y))
      for (i <- 0 to expand(grid.x) - 1; j <- 0 to expand(grid.y) - 1) {
        vm(i)(j) = " "
      }
      for (i <- 0 to grid.x - 1; j <- 0 to grid.y - 1) {
        vm(expand(i))(expand(j)) = "p"
      }
      vm(expand(way.head.x))(expand(way.head.y)) = "E"
      vm(expand(way.last.x))(expand(way.last.y)) = "S"
      way zip way.tail map( n => vm(path(n._1.x) + path(n._2.x) + 1)(path(n._1.y) + path(n._2.y) + 1) = "+")
      vm.map(_.fold("")((z, n) => z + " " + n + " ")).fold("")((z, n) => z + n + "\n") 
    }

    def printResult(way: List[Point], grid: Grid, nrOfGenerations: Int) {
      log.info("Generation:" + nrOfGenerations)
      log.info("steps:" + ( way.length - 1))
      log.info("turn:" + turnCount(way))
      log.info(way.reverse.toString)
      log.info(VisibleGrid(way, grid))
    }

  }

}
