package com.example

import cats.Show
import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.example.Prng.Seed
import com.example.samegame.SameGame

object Main extends IOApp {
  private val position  = data.Games.board(7)
  private val score     = SameGame.score(position)
  private val gameState = GameState(playedMoves = List.empty[samegame.Position], score = score, position = position)

  private implicit val showResult: Show[GameState[samegame.Position, samegame.Game, Int]] = Interpreters.showResult

  private def putStrLn[T: Show](t: T): IO[Unit] = IO(println(show"$t"))

  val resultState: IO[Unit] = {
    implicit val logger: Logger[StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], ?]] =
      Interpreters.loggerState

    val interpreter  = Interpreters.withState()
    val initialState = SearchState(Seed(234924L), gameState, None, None)
    for {
      result <- Search.nestedMonteCarlo(3, interpreter).runS(initialState)
      _      <- putStrLn(result.gameState)
    } yield ()
  }

  val resultIORef: IO[Unit] = {
    val initialState                = SearchState((), gameState, None, None)
    implicit val logger: Logger[IO] = Interpreters.loggerIORef
    for {
      interpreter <- Interpreters.withIORef(initialState)
      result      <- Search.nestedMonteCarlo(2, interpreter) *> interpreter.gameState
      _           <- putStrLn(result)
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    // resultIORef.as(ExitCode.Success)
    resultState.as(ExitCode.Success)
}
