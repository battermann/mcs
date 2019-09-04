package mcs

import cats.{Eq, Show}
import cats.effect.IO
import cats.implicits._
import mcs.samegame._
import mcs.util.ListUtils

object Interpreters {

  type Move          = samegame.Position
  type BoardPosition = samegame.Game

  implicit val game: Game[IO, Move, BoardPosition, Int] = new Game[IO, Move, BoardPosition, Int] {
    def applyMove(gameState: GameState[Move, BoardPosition, Int], move: Move): GameState[Move, BoardPosition, Int] = {
      val gs = samegame.SameGame.applyMove(move, gameState.position)
      GameState(move :: gameState.playedMoves, samegame.SameGame.score(gs), gs)
    }

    def legalMoves(gameState: GameState[Move, BoardPosition, Int]): List[Move] =
      samegame.SameGame.legalMoves(gameState.position)

    def simulation(gameState: GameState[Move, BoardPosition, Int]): IO[GameState[Move, BoardPosition, Int]] = {
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
    }

    def isPrefixOf: List[Move] => List[Move] => Boolean = currentPath => bestPath => ListUtils.isSuffixOf(currentPath, bestPath)(Eq.fromUniversalEquals)

    def next(currentPath: List[Move], bestPath: List[Move]): Option[Move] =
      if (isPrefixOf(currentPath)(bestPath) && currentPath.length < bestPath.length) {
        bestPath(bestPath.length - 1 - currentPath.length).some
      }
      else {
        None
      }
  }

  implicit val loggerIO: Logger[IO] = new Logger[IO] {
    def log[T: Show](t: T): IO[Unit] = IO(println(t.show))
  }

  implicit val showCell: Show[CellState] = Show.show {
    case Empty         => "-"
    case Filled(Green) => "0"
    case Filled(Blue)  => "1"
    case Filled(Red)   => "2"
    case Filled(Brown) => "3"
    case Filled(Gray)  => "4"
  }

  implicit val showMove: Show[Move] =
    Show.show(p => show"(${p.col}, ${p.row})")

  implicit val showList: Show[List[Move]] =
    Show.show(_.map(_.show).mkString("[", ", ", "]"))

  implicit val showResult: Show[Result[Move, Int]] =
    Show.show(result => show">>> Improved sequence found\n>>> Score: ${result.score.show}, Moves: ${result.moves.reverse.show}")

  implicit val showBoard: Show[Board] =
    Show.show(_.columns.map(col => col.cells.map(_.show).reverse).transpose.map(_.mkString("[", ",", "]")).mkString("\n"))

  implicit val showGame: Show[BoardPosition] = Show.show {
    case InProgress(board, score) => show"$board\n\nScore: $score (game in progress)"
    case Finished(board, score)   => show"$board\n\nScore: $score (game finished)"
  }

  implicit val showGameState: Show[GameState[Move, BoardPosition, Int]] = Show.show(t => show"""
       |${t.position}
       |
       |Moves: ${t.playedMoves.reverse}
       |""".stripMargin)

  val showGameStateAsQueryParams: Show[GameState[Move, BoardPosition, Int]] =
    Show.show(t => show"""Moves: ${t.playedMoves.reverse.map(p => s"move=${p.col}%2C${p.row}").mkString("&")}
                   |
                   |Score: ${t.score}
                   |""".stripMargin)

  val showGameStateAsJsFunctionCalls: Show[GameState[Move, BoardPosition, Int]] =
    Show.show(t => show"""Moves: ${t.playedMoves.reverse.map(p => s"sg_remove(${p.col},${14 - p.row})").mkString(";")}
                  |
                  |Score: ${t.score}
                  |""".stripMargin)

  implicit val positionEq: Eq[Move] = Eq.fromUniversalEquals
}
