package mcs

import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Interpreters._
import mcs.Prng.Seed
import mcs.samegame.SameGame

import scala.concurrent.ExecutionContext

object Main extends IOApp {
  implicit val ctx: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  private val (position, _) = data.Games.board(7)
  private val score         = SameGame.score(position)
  private val gameState     = GameState(playedMoves = List.empty[Move], score = score, position = position)

  val search1: IO[Unit] = {
    implicit val logger: Logger[StateIO] = Interpreters.loggerState
    implicit val interpreter: Game[StateIO, Move, BoardPosition, Int, Seed] =
      Interpreters.gameInterpreterStateT

    for {
      cores <- IO(Runtime.getRuntime.availableProcessors())
      seeds <- List.fill(cores)(IO(Seed(scala.util.Random.nextLong()))).sequence
      results <- seeds
        .parTraverse { seed =>
          Programs.nestedMonteCarlo[StateIO, Move, BoardPosition, Int, Seed](2).runA(SearchState(seed, gameState, None, None))
        }
      _ <- IO(println(show"""Best result:\n${results.maxBy(_.score)}"""))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    search1.as(ExitCode.Success)
}
