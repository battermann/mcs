package mcs

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Interpreters._
import mcs.Prng.Seed
import mcs.samegame.SameGame

object Main extends IOApp {
  private val (position, _) = data.Games.board(7)
  private val score         = SameGame.score(position)
  private val gameState     = GameState(playedMoves = List.empty[Move], score = score, position = position)

  val search1: IO[Unit] = {
    implicit val logger: Logger[StateIO] = Interpreters.loggerState
    implicit val interpreter: Game[StateIO, Move, BoardPosition, Int, Seed] =
      Interpreters.gameInterpreterStateT

    val initialState = SearchState(Seed(101L), gameState, None, None)
    Programs.nestedMonteCarlo[StateIO, Move, BoardPosition, Int, Seed](3).runA(initialState)
  }

  val search2: IO[Unit] = {
    implicit val logger: Logger[IO] = Interpreters.loggerIORef

    val initialState = SearchState((), gameState, None, None)
    Interpreters
      .gameInterpreterIORef(initialState)
      .flatMap(implicit ev => Programs.nestedMonteCarlo[IO, Move, BoardPosition, Int, Unit](3))
  }

  def run(args: List[String]): IO[ExitCode] =
    search1.as(ExitCode.Success)
}
