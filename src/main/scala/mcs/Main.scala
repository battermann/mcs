package mcs

import cats.{Eq, Show}
import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Prng.Seed
import mcs.samegame.SameGame

object Main extends IOApp {
  private val (position, _) = data.Games.board(7)
  private val score         = SameGame.score(position)
  private val gameState     = GameState(playedMoves = List.empty[samegame.Position], score = score, position = position)

  private implicit val showGameState: Show[GameState[samegame.Position, samegame.Game, Int]] = Interpreters.showGameState
  private implicit val showResult: Show[Result[samegame.Position, Int]]                      = Interpreters.showResult
  implicit val positionEq: Eq[samegame.Position]                                             = Eq.fromUniversalEquals

  val search1: IO[Unit] = {
    implicit val logger: Logger[StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], ?]] =
      Interpreters.loggerState

    val interpreter  = Interpreters.gameInterpreterStateT
    val initialState = SearchState(Seed(101L), gameState, None, None)
    Programs.nestedMonteCarlo(interpreter, 3).runA(initialState)
  }

  val search2: IO[Unit] = {
    val initialState                = SearchState((), gameState, None, None)
    implicit val logger: Logger[IO] = Interpreters.loggerIORef
    for {
      interpreter <- Interpreters.gameInterpreterIORef(initialState)
      _           <- Programs.nestedMonteCarlo(interpreter, 3)
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    search1.as(ExitCode.Success)
}
