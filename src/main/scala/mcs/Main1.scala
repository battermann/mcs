//package mcs
//
//import cats.effect.concurrent.Ref
//import cats.effect.{ContextShift, ExitCode, IO, IOApp}
//import mcs.Interpreters._
//import mcs.samegame.SameGame
//import cats.implicits._
//import scala.concurrent.ExecutionContext
//import scala.util.Try
//
//object Main1 extends IOApp {
//  private implicit val ctx: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
//  private implicit val logger: Logger[IO]    = Interpreters.loggerIO
//
//  private val (position, best) = data.Games.jsGames10
//  private val score            = SameGame.score(position)
//  private val gameState        = GameState(playedMoves = List.empty[Move], score = score, position = position)
//
//  private def startSearch(level: Int): IO[Unit] = {
//    for {
//      cores <- IO(Runtime.getRuntime.availableProcessors())
//      _     <- IO(println(s"Available processors: $cores"))
//      _     <- IO(println(s"Nesting level: $level"))
//      ref   <- Ref.of[IO, Option[Result[Move, Int]]](None)
//      results <- List.fill(cores)(()).parTraverse { _ =>
//        Interpreters
//          .gameInterpreterIORef(SearchState((), gameState, best))
//          .flatMap { implicit ev =>
//            Programs.nestedMonteCarlo[IO, Move, BoardPosition, Int](ref, level)
//          }
//      }
//      _ <- IO(println(show"""\nBest result:\n${results.maxBy(_.score)}"""))
//    } yield ()
//  }
//
//  def run(args: List[String]): IO[ExitCode] =
//    startSearch(args.headOption.flatMap(arg => Try(arg.toInt).toOption).getOrElse(1)).as(ExitCode.Success)
//}
