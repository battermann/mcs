package mcs

import cats.data.StateT
import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Interpreters._
import mcs.Prng.Seed
import mcs.samegame.SameGame

import scala.concurrent.ExecutionContext

object Main extends IOApp {
  private implicit val ctx: ContextShift[IO]   = IO.contextShift(ExecutionContext.global)
  private implicit val logger: Logger[StateIO] = Interpreters.loggerState
  private implicit val interpreter: Game[StateIO, Move, BoardPosition, Int, Seed] =
    Interpreters.gameInterpreterStateT

  private val level         = 3
  private val (position, _) = data.Games.board(7)
  private val score         = SameGame.score(position)
  private val gameState     = GameState(playedMoves = List.empty[Move], score = score, position = position)

  private val startSearch: IO[Unit] = {
    for {
      cores <- IO(Runtime.getRuntime.availableProcessors())
      seeds <- List.fill(cores)(IO(Seed(scala.util.Random.nextLong()))).sequence
      ref   <- Ref.of[IO, Option[Result[Move, Int]]](None)
      results <- seeds
        .parTraverse { seed =>
          Programs
            .nestedMonteCarlo[StateIO, Move, BoardPosition, Int, Seed](ref.mapK[StateIO](StateT.liftK), level)
            .runA(SearchState(seed, gameState, None, None))
        }
      _ <- IO(println(show"""\nBest result:\n${results.maxBy(_.score)}"""))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    startSearch.as(ExitCode.Success)
}
