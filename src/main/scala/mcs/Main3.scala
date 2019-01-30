package mcs

import cats.Show
import cats.effect.{ContextShift, IO}
import cats.implicits._

import scala.concurrent.ExecutionContext

object Main3 {

  trait Game2[Move, Position, Score] {
    def applyMove(gameState: GameState[Move, Position, Score], move: Move): GameState[Move, Position, Score]
    def legalMoves(gameState: GameState[Move, Position, Score]): List[Move]
    def simulation(gameState: GameState[Move, Position, Score]): IO[GameState[Move, Position, Score]]
  }

  private implicit val ctx: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  def search[Move, Position, Score](numLevels: Int, level: Int, gameState: GameState[Move, Position, Score])(
      implicit g: Game2[Move, Position, Score],
      ord: Ordering[Score],
      show: Show[GameState[Move, Position, Score]]): IO[GameState[Move, Position, Score]] = {
    val legalMoves = g.legalMoves(gameState)
    if (level == numLevels) println(gameState.show)
    if (legalMoves.isEmpty) {
      IO.pure(gameState)
    } else {
      if (level == 1) {
        legalMoves
          .traverse { move =>
            val nextGameState = g.applyMove(gameState, move)
            g.simulation(nextGameState).map((move, _))
          }
          .map(_.maxBy(_._2.score))
          .flatMap { case (move, _) => search[Move, Position, Score](numLevels, level, g.applyMove(gameState, move)) }
      } else if (level == 2) {
        legalMoves
          .parTraverse { move =>
            val nextState = g.applyMove(gameState, move)
            search[Move, Position, Score](numLevels, level - 1, nextState).map((move, _))
          }
          .map(_.maxBy(_._2.score))
          .flatMap { case (move, _) => search[Move, Position, Score](numLevels, level, g.applyMove(gameState, move)) }
      } else {
        legalMoves
          .traverse { move =>
            val nextState = g.applyMove(gameState, move)
            search[Move, Position, Score](numLevels, level - 1, nextState).map((move, _))
          }
          .map(_.maxBy(_._2.score))
          .flatMap { case (move, _) => search[Move, Position, Score](numLevels, level, g.applyMove(gameState, move)) }
      }
    }
  }

  implicit val game2: Game2[samegame.Position, samegame.Game, Int] = new Game2[samegame.Position, samegame.Game, Int] {
    def applyMove(gameState: GameState[samegame.Position, samegame.Game, Int], move: samegame.Position): GameState[samegame.Position, samegame.Game, Int] = {
      val gs = samegame.SameGame.applyMove(move, gameState.position)
      GameState(move :: gameState.playedMoves, samegame.SameGame.score(gs), gs)
    }

    def legalMoves(gameState: GameState[samegame.Position, samegame.Game, Int]): List[samegame.Position] =
      samegame.SameGame.legalMoves(gameState.position)

    def simulation(gameState: GameState[samegame.Position, samegame.Game, Int]): IO[GameState[samegame.Position, samegame.Game, Int]] = {
      val tabuColor = samegame.SameGame.predominantColor(gameState.position)
      val moves     = legalMoves(gameState)
      moves.partition(m => samegame.SameGame.color(gameState.position, m) == tabuColor) match {
        case (Nil, Nil) => IO.pure(gameState)
        case (tabuMoves, Nil) =>
          IO(scala.util.Random.nextInt(tabuMoves.length))
            .map(i => applyMove(gameState, tabuMoves(i)))
            .flatMap(gs => simulation(gs))
        case (_, ms) =>
          IO(scala.util.Random.nextInt(ms.length))
            .map(i => applyMove(gameState, ms(i)))
            .flatMap(gs => simulation(gs))
      }
      if (moves.isEmpty) {
        IO.pure(gameState)
      } else {
        IO(scala.util.Random.nextInt(moves.length))
          .map(i => applyMove(gameState, moves(i)))
          .flatMap(gs => simulation(gs))
      }
    }
  }

  val (position, _) = data.Games.board(7)
  val score         = samegame.SameGame.score(position)
  val gameState     = GameState(playedMoves = List.empty[samegame.Position], score = score, position = position)

  import Interpreters.showGameState

  val level = 3
  search[samegame.Position, samegame.Game, Int](level, level, gameState)
    .flatMap(r => IO(println(r.show)))
    .unsafeRunSync()
}
