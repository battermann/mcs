package mcs

import cats.Eq
import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Interpreters._
import mcs.Prng.Seed
import mcs.samegame.SameGame

object Main extends IOApp {
  private val (position, _) = data.Games.board(7)
  private val score         = SameGame.score(position)
  private val gameState     = GameState(playedMoves = List.empty[Move], score = score, position = position)

  implicit val positionEq: Eq[Move] = Eq.fromUniversalEquals

  val search1: IO[Unit] = {
    type StateIO[A] = StateT[IO, SearchState[Move, BoardPosition, Int, Seed], A]
    implicit val logger: Logger[StateIO] = Interpreters.loggerState
    implicit val interpreter: Game[StateIO, Move, BoardPosition, Int, Seed] =
      Interpreters.gameInterpreterStateT
    val initialState = SearchState(Seed(101L), gameState, None, None)
    Programs.nestedMonteCarlo[StateIO, Move, BoardPosition, Int, Seed](3).runA(initialState)
  }

  val search2: IO[Unit] = {
    val initialState                = SearchState((), gameState, None, None)
    implicit val logger: Logger[IO] = Interpreters.loggerIORef
    Interpreters
      .gameInterpreterIORef(initialState)
      .flatMap(implicit ev => Programs.nestedMonteCarlo[IO, Move, BoardPosition, Int, Unit](3))
  }

  def run(args: List[String]): IO[ExitCode] =
    search1.as(ExitCode.Success)
}
