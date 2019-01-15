package mcs

import cats.Show
import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Prng.Seed
import mcs.samegame.SameGame

object Main extends IOApp {
  private val position  = data.Games.board(7)
  private val score     = SameGame.score(position)
  private val gameState = GameState(playedMoves = List.empty[samegame.Position], score = score, position = position)

  private implicit val showGameState: Show[GameState[samegame.Position, samegame.Game, Int]] = Interpreters.showGameState
  private implicit val showResult: Show[Result[samegame.Position, Int]]                      = Interpreters.showResult

  val resultState: IO[Unit] = {
    implicit val logger: Logger[StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], ?]] =
      Interpreters.loggerState

    val interpreter  = Interpreters.gameInstanceStateT()
    val initialState = SearchState(Seed(-1L), gameState, None, None)
    Programs.nestedMonteCarlo(3, interpreter).runA(initialState)
  }

  val resultIORef: IO[Unit] = {
    val initialState                = SearchState((), gameState, None, None)
    implicit val logger: Logger[IO] = Interpreters.loggerIORef
    for {
      interpreter <- Interpreters.gameInstanceIORef(initialState)
      _           <- Programs.nestedMonteCarlo(2, interpreter)
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    // resultIORef.as(ExitCode.Success)
    resultState.as(ExitCode.Success)
}
