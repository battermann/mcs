package mcs

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import mcs.Interpreters._
import mcs.Programs.SearchState
import mcs.samegame.SameGame

import scala.util.Try

object Main extends IOApp {
  private val (position, best) = data.Games.board(15)
  private val score            = SameGame.score(position)
  private val gameState        = GameState(playedMoves = List.empty[Move], score = score, position = position)

  private def startSearch(level: Int): IO[Unit] = {
    for {
      cores <- IO(Runtime.getRuntime.availableProcessors())
      _     <- IO(println(s"Available processors: $cores"))
      _     <- IO(println(s"Nesting level: $level"))
      ref   <- Ref.of[IO, Option[Result[Move, Int]]](None)
      _     <- Programs.nestedMonteCarlo[IO, IO.Par, Move, BoardPosition, Int](SearchState(gameState, best), ref, level)
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    startSearch(args.headOption.flatMap(arg => Try(arg.toInt).toOption).getOrElse(1)).as(ExitCode.Success)
}
