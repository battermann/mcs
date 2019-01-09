package com.example

import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.example.Prng.Seed
import com.example.samegame.SameGame

final case class GameState[Move, Position, Score](
    playedMoves: List[Move],
    score: Score,
    position: Position,
)

final case class Result[Move, Score](
    moves: List[Move],
    score: Score
)

final case class SearchState[Move, Position, Score](
    seed: Seed,
    gameState: GameState[Move, Position, Score],
    bestResult: Option[Result[Move, Score]]
)

trait Game[F[_], Move, Position, Score] {
  type S = SearchState[Move, Position, Score]

  def applyMove(move: Move): F[Unit]
  def legalMoves: F[List[Move]]
  def rndInt(bound: Int): F[Int]
  def rndSimulation(): F[Unit]

  def set(searchState: S): F[Unit]
  def get(): F[S]
  def inspect[A](f: S => A): F[A]
  def modify(f: S => S): F[Unit]
  def pure[A](a: A): F[A]
}

object Game {
  def apply[F[_], Move, Position, Score]()(implicit ev: Game[F, Move, Position, Score]): Game[F, Move, Position, Score] = ev
}

object Search {

  def simpleMonteCarlo[F[_]: Monad, Move, Position, Score](game: Game[F, Move, Position, Score])(implicit ord: Ordering[Score]): F[Unit] = {

    val playMoveWithBestSimulationResult = for {
      moves        <- game.legalMoves
      currentState <- game.inspect(_.gameState)
      isTerminalPosition <- moves match {
        case Nil => game.pure(true)
        case ms =>
          ms.traverse { m =>
              for {
                _         <- game.modify(_.copy(gameState = currentState))
                _         <- game.applyMove(m)
                nextState <- game.inspect(_.gameState)
                _         <- game.rndSimulation()
                simResult <- game.inspect(_.gameState)
              } yield (simResult, nextState)
            }
            .map(_.maxBy(_._1.score))
            .flatMap {
              case (simResult, nextState) =>
                game
                  .modify {
                    case st @ SearchState(_, _, None) =>
                      st.copy(gameState = nextState, bestResult = Result(simResult.playedMoves, simResult.score).some)
                    case st @ SearchState(_, _, Some(result)) =>
                      if (ord.gt(simResult.score, result.score)) {
                        st.copy(gameState = nextState, bestResult = Result(simResult.playedMoves, simResult.score).some)
                      } else {
                        st.copy(gameState = nextState, bestResult = result.some)
                      }
                  }
                  .as(false)
            }
      }
    } yield isTerminalPosition

    playMoveWithBestSimulationResult.iterateUntil(b => b).void
  }
}

object Main extends IOApp {

  private val game  = data.Games.game1
  private val score = SameGame.score(game)
  private val s     = SearchState(Seed(5L), GameState(List.empty[samegame.Position], score, game), None)

  val resultState: IO[Unit] = {
    val instance = Instances.createState()
    val Some(r)  = Search.simpleMonteCarlo(instance).runS(s).value.bestResult
    IO(println(r.moves.reverse.map(p => s"(${p.col}, ${p.row})").mkString("[", ", ", "]"))) *> IO(println(s"Score: ${r.score}"))
  }

  val resultIORef: IO[Unit] = for {
    instance    <- Instances.createIORef(s)
    _           <- Search.simpleMonteCarlo(instance)
    maybeResult <- instance.inspect(_.bestResult)
    _ <- maybeResult match {
      case Some(r) =>
        IO(println(r.moves.reverse.map(p => s"(${p.col}, ${p.row})").mkString("[", ", ", "]"))) *> IO(println(s"Score: ${r.score}"))
      case None =>
        ().pure[IO]
    }
  } yield ()

  def run(args: List[String]): IO[ExitCode] =
    resultIORef.as(ExitCode.Success)
  // resultState.as(ExitCode.Success)
}
