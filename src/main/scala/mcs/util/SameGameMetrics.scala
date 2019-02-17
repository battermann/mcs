package mcs.util

import mcs.samegame._

import scala.annotation.tailrec
import cats.implicits._
import breeze.stats._

object SameGameMetrics {
  // some impure code to try stuff

  type Result    = mcs.Result[Position, Int]
  type GameState = mcs.GameState[Position, Game, Int]

  @tailrec
  private def random(gameState: GameState, moveStats: Map[Int, List[Double]]): (GameState, Map[Int, List[Double]]) = {
    val moves = SameGame.legalMoves(gameState.position)
    moves match {
      case Nil => (gameState, moveStats)
      case _ =>
        val i            = scala.util.Random.nextInt(moves.length)
        val move         = moves(i)
        val gs           = SameGame.applyMove(move, gameState.position)
        val updatedStats = moveStats |+| Map(gameState.playedMoves.length -> List(moves.length.toDouble))
        random(mcs.GameState(move :: gameState.playedMoves, SameGame.score(gs), gs), updatedStats)
    }
  }

  private val (position, _) = mcs.data.Games.jsGames10
  private val score         = SameGame.score(position)
  private val gameState     = mcs.GameState(playedMoves = List.empty[Position], score = score, position = position)

  def runRndSimulationsAndPrintMetrics(n: Int): Unit = {
    val init = Map.empty[Int, List[Double]]
    val stats = (0 until n).foldLeft(init) {
      case (acc, _) =>
        random(gameState, acc)._2
    }

    val withStats = stats.mapValues(xs => meanAndVariance(xs))

    withStats.toList.sortBy(_._1).foreach {
      case (num, meanAndVar) =>
        println(s"""move: $num
         |  - avg number of child nodes: ${meanAndVar.mean.toInt} (variance: ${f"${meanAndVar.variance}%2.2f"} count: ${meanAndVar.count})
       """.stripMargin)
    }

    val complexity = withStats.values.map(_.mean).foldLeft(1.0) {
      case (acc, x) =>
        acc * x
    }

    println(s"complexity: ~10^${math.log10(complexity).toInt}")
  }

  /*
  scala> mcs.util.SameGameMetrics.runRndSimulationsAndPrintMetrics(20)
move: 0
  - avg number of child nodes: 45 (variance: 0.00 count: 20)

move: 1
  - avg number of child nodes: 44 (variance: 2.56 count: 20)

move: 2
  - avg number of child nodes: 43 (variance: 3.27 count: 20)

move: 3
  - avg number of child nodes: 42 (variance: 2.27 count: 20)

move: 4
  - avg number of child nodes: 42 (variance: 2.37 count: 20)

move: 5
  - avg number of child nodes: 41 (variance: 2.45 count: 20)

move: 6
  - avg number of child nodes: 40 (variance: 3.04 count: 20)

move: 7
  - avg number of child nodes: 40 (variance: 6.05 count: 20)

move: 8
  - avg number of child nodes: 39 (variance: 5.82 count: 20)

move: 9
  - avg number of child nodes: 38 (variance: 4.47 count: 20)

move: 10
  - avg number of child nodes: 37 (variance: 3.84 count: 20)

move: 11
  - avg number of child nodes: 36 (variance: 4.68 count: 20)

move: 12
  - avg number of child nodes: 35 (variance: 3.08 count: 20)

move: 13
  - avg number of child nodes: 34 (variance: 3.71 count: 20)

move: 14
  - avg number of child nodes: 34 (variance: 3.01 count: 20)

move: 15
  - avg number of child nodes: 32 (variance: 4.26 count: 20)

move: 16
  - avg number of child nodes: 32 (variance: 3.61 count: 20)

move: 17
  - avg number of child nodes: 31 (variance: 3.99 count: 20)

move: 18
  - avg number of child nodes: 31 (variance: 4.91 count: 20)

move: 19
  - avg number of child nodes: 29 (variance: 4.98 count: 20)

move: 20
  - avg number of child nodes: 28 (variance: 6.77 count: 20)

move: 21
  - avg number of child nodes: 28 (variance: 7.00 count: 20)

move: 22
  - avg number of child nodes: 27 (variance: 5.80 count: 20)

move: 23
  - avg number of child nodes: 26 (variance: 6.98 count: 20)

move: 24
  - avg number of child nodes: 25 (variance: 5.31 count: 20)

move: 25
  - avg number of child nodes: 25 (variance: 6.34 count: 20)

move: 26
  - avg number of child nodes: 24 (variance: 4.80 count: 20)

move: 27
  - avg number of child nodes: 23 (variance: 6.01 count: 20)

move: 28
  - avg number of child nodes: 22 (variance: 6.16 count: 20)

move: 29
  - avg number of child nodes: 22 (variance: 7.29 count: 20)

move: 30
  - avg number of child nodes: 21 (variance: 8.45 count: 20)

move: 31
  - avg number of child nodes: 21 (variance: 8.45 count: 20)

move: 32
  - avg number of child nodes: 20 (variance: 8.77 count: 20)

move: 33
  - avg number of child nodes: 20 (variance: 6.43 count: 20)

move: 34
  - avg number of child nodes: 19 (variance: 6.26 count: 20)

move: 35
  - avg number of child nodes: 18 (variance: 5.78 count: 20)

move: 36
  - avg number of child nodes: 17 (variance: 4.53 count: 20)

move: 37
  - avg number of child nodes: 17 (variance: 6.16 count: 20)

move: 38
  - avg number of child nodes: 16 (variance: 4.79 count: 20)

move: 39
  - avg number of child nodes: 15 (variance: 4.26 count: 20)

move: 40
  - avg number of child nodes: 15 (variance: 5.21 count: 20)

move: 41
  - avg number of child nodes: 14 (variance: 6.37 count: 20)

move: 42
  - avg number of child nodes: 13 (variance: 7.31 count: 20)

move: 43
  - avg number of child nodes: 12 (variance: 8.24 count: 20)

move: 44
  - avg number of child nodes: 12 (variance: 6.72 count: 20)

move: 45
  - avg number of child nodes: 11 (variance: 7.31 count: 20)

move: 46
  - avg number of child nodes: 10 (variance: 5.75 count: 20)

move: 47
  - avg number of child nodes: 10 (variance: 9.10 count: 20)

move: 48
  - avg number of child nodes: 10 (variance: 9.73 count: 20)

move: 49
  - avg number of child nodes: 9 (variance: 7.94 count: 20)

move: 50
  - avg number of child nodes: 9 (variance: 8.34 count: 20)

move: 51
  - avg number of child nodes: 8 (variance: 6.48 count: 20)

move: 52
  - avg number of child nodes: 7 (variance: 7.50 count: 20)

move: 53
  - avg number of child nodes: 7 (variance: 6.64 count: 20)

move: 54
  - avg number of child nodes: 6 (variance: 5.32 count: 20)

move: 55
  - avg number of child nodes: 5 (variance: 5.47 count: 20)

move: 56
  - avg number of child nodes: 5 (variance: 6.16 count: 20)

move: 57
  - avg number of child nodes: 4 (variance: 6.15 count: 20)

move: 58
  - avg number of child nodes: 4 (variance: 5.79 count: 18)

move: 59
  - avg number of child nodes: 4 (variance: 5.09 count: 18)

move: 60
  - avg number of child nodes: 3 (variance: 4.13 count: 17)

move: 61
  - avg number of child nodes: 3 (variance: 2.97 count: 13)

move: 62
  - avg number of child nodes: 3 (variance: 2.09 count: 12)

move: 63
  - avg number of child nodes: 2 (variance: 1.76 count: 11)

move: 64
  - avg number of child nodes: 2 (variance: 1.29 count: 11)

move: 65
  - avg number of child nodes: 2 (variance: 1.36 count: 8)

move: 66
  - avg number of child nodes: 2 (variance: 1.57 count: 7)

move: 67
  - avg number of child nodes: 2 (variance: 0.97 count: 6)

move: 68
  - avg number of child nodes: 2 (variance: 0.25 count: 4)

move: 69
  - avg number of child nodes: 2 (variance: 0.67 count: 4)

move: 70
  - avg number of child nodes: 1 (variance: 0.92 count: 4)

move: 71
  - avg number of child nodes: 2 (variance: 2.00 count: 2)

move: 72
  - avg number of child nodes: 2 (variance: 0.00 count: 1)

move: 73
  - avg number of child nodes: 1 (variance: 0.00 count: 1)

complexity: ~10^82
 */
}
