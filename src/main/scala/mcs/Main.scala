package mcs

import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Interpreters._
import mcs.Programs.SearchState
import mcs.samegame.SameGame

import scala.util.Try

object Main extends IOApp {
  private val threadPool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())

  override protected implicit def contextShift: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.fromExecutor(threadPool))

  private val (position, best) = data.Games.jsGames10
  private val score            = SameGame.score(position)
  private val gameState        = GameState(playedMoves = List.empty[Move], score = score, position = position)

  private def startSearch(level: Int, best: Option[Result[samegame.Position, Int]]): IO[Unit] =
    for {
      cores <- IO(Runtime.getRuntime.availableProcessors())
      _     <- IO(println(s"Available processors: $cores"))
      _     <- IO(println(s"Nesting level: $level"))
      ref   <- Ref.of[IO, Option[Result[Move, Int]]](None)
      _     <- Programs.nestedMonteCarloTreeSearch[IO, IO.Par, Move, BoardPosition, Int](SearchState(gameState, best), ref, level)
      _     <- IO(threadPool.shutdown())
    } yield ()

  def run(args: List[String]): IO[ExitCode] =
    startSearch(args.headOption.flatMap(arg => Try(arg.toInt).toOption).getOrElse(1), args.drop(1).headOption.filter(_ == "best").flatMap(_ => best))
      .as(ExitCode.Success)
}
