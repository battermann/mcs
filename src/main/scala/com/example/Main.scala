package com.example

import cats.{Monad, Show}
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
  def rndSimulation: F[Unit]
  def gameState: F[GameState[Move, Position, Score]]
  def bestResult: F[Option[Result[Move, Score]]]
  def update(f: S => S): F[Unit]
  def pure[A](a: A): F[A]
}

object Game {
  def apply[F[_], Move, Position, Score]()(implicit ev: Game[F, Move, Position, Score]): Game[F, Move, Position, Score] = ev
}

object Search {
  def updateBestResultAndSetNextState[Move, Position, Score](simResult: GameState[Move, Position, Score], nextState: GameState[Move, Position, Score])(
      searchState: SearchState[Move, Position, Score])(implicit ord: Ordering[Score]): SearchState[Move, Position, Score] =
    searchState match {
      case SearchState(seed, _, None) =>
        SearchState(seed = seed, gameState = nextState, bestResult = Result(simResult.playedMoves, simResult.score).some)
      case SearchState(seed, _, Some(result)) =>
        if (ord.gt(simResult.score, result.score)) {
          SearchState(seed = seed, gameState = nextState, bestResult = Result(simResult.playedMoves, simResult.score).some)
        } else {
          SearchState(seed = seed, gameState = nextState, bestResult = result.some)
        }
    }

  def simpleMonteCarlo[F[_]: Monad, Move, Position, Score](game: Game[F, Move, Position, Score])(implicit ord: Ordering[Score]): F[Unit] = {
    val playMoveWithBestSimulationResult = for {
      moves        <- game.legalMoves
      currentState <- game.gameState
      isTerminalPosition <- moves match {
        case Nil => game.pure(true)
        case ms =>
          ms.traverse { m =>
              for {
                nextState <- game.update(_.copy(gameState = currentState)) *> game.applyMove(m) *> game.gameState
                simResult <- game.rndSimulation *> game.gameState
              } yield (simResult, nextState)
            }
            .map(_.maxBy(_._1.score))
            .flatMap { case (simResult, nextState) => game.update(updateBestResultAndSetNextState(simResult, nextState)).as(false) }
      }
    } yield isTerminalPosition

    playMoveWithBestSimulationResult.iterateUntil(isTerminalPosition => isTerminalPosition).void
  }
}

object Main extends IOApp {
  private val game  = data.Games.game1
  private val score = SameGame.score(game)
  private val s     = SearchState(Seed(5L), GameState(List.empty[samegame.Position], score, game), None)

  private implicit val showResult: Show[Option[Result[samegame.Position, Int]]] = Instances.showResult

  private def putStrLn[T: Show](t: T): IO[Unit] = IO(println(show"$t"))

  val resultState: IO[Unit] = {
    val instance = Instances.withState()
    val result   = Search.simpleMonteCarlo(instance).runS(s).value.bestResult
    putStrLn(result)
  }

  val resultIORef: IO[Unit] = for {
    instance <- Instances.withIORef(s)
    result   <- Search.simpleMonteCarlo(instance) *> instance.bestResult
    _        <- putStrLn(result)
  } yield ()

  def run(args: List[String]): IO[ExitCode] =
    // resultIORef.as(ExitCode.Success)
    resultState.as(ExitCode.Success)
}
