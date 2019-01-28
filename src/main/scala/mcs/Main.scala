package mcs

import cats.data.StateT
import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Interpreters._
import mcs.Prng.Seed
import mcs.samegame.SameGame

import scala.concurrent.ExecutionContext
import scala.util.Try

object Main extends IOApp {
  private implicit val ctx: ContextShift[IO]   = IO.contextShift(ExecutionContext.global)
  private implicit val logger: Logger[StateIO] = Interpreters.loggerState
  private implicit val interpreter: Game[StateIO, Move, BoardPosition, Int, Seed] =
    Interpreters.gameInterpreterStateT

  private val (position, _) = data.Games.jsGames10
  private val score         = SameGame.score(position)
  private val gameState     = GameState(playedMoves = List.empty[Move], score = score, position = position)

  private def startSearch(level: Int): IO[Unit] = {
    for {
      cores <- IO(Runtime.getRuntime.availableProcessors())
      _     <- IO(println(s"Available processors: $cores"))
      _     <- IO(println(s"Nesting level: $level"))
      seeds <- List.fill(cores)(IO(scala.util.Random.nextLong())).sequence
      ref   <- Ref.of[IO, Option[Result[Move, Int]]](None)
      results <- seeds
        .map(Seed)
        .parTraverse { seed =>
          Programs
            .nestedMonteCarlo[StateIO, Move, BoardPosition, Int, Seed](ref.mapK[StateIO](StateT.liftK), level)
            .runA(SearchState(seed, gameState, None))
        }
      _ <- IO(println(show"""\nBest result:\n${results.maxBy(_.score)}"""))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    startSearch(args.headOption.flatMap(arg => Try(arg.toInt).toOption).getOrElse(1)).as(ExitCode.Success)
}
