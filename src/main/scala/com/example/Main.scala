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
    bestSequence: Option[Result[Move, Score]],
    bestTotal: Option[Result[Move, Score]]
)

trait Game[F[_], Move, Position, Score] {
  type S = SearchState[Move, Position, Score]

  def applyMove(move: Move): F[Unit]
  def update(f: S => S): F[Unit]
  def simulation: F[Unit]
  def log(msg: String): F[Unit]

  def legalMoves: F[List[Move]]
  def rndInt(bound: Int): F[Int]
  def gameState: F[GameState[Move, Position, Score]]
  def bestSequence: F[Option[Result[Move, Score]]]
  def bestTotal: F[Option[Result[Move, Score]]]
}

object Game {
  def apply[F[_], Move, Position, Score]()(implicit ev: Game[F, Move, Position, Score]): Game[F, Move, Position, Score] = ev
}

object Search {
  def updateSearchState[F[_]: Monad, Move, Position, Score](
      game: Game[F, Move, Position, Score],
      currentState: GameState[Move, Position, Score],
      nextState: GameState[Move, Position, Score],
      currentBestSequence: Option[Result[Move, Score]],
      simResult: GameState[Move, Position, Score])(implicit ord: Ordering[Score], showResult: Show[GameState[Move, Position, Score]]): F[Unit] =
    for {
      _ <- currentBestSequence match {
        case None =>
          game.update(_.copy(gameState = nextState, bestSequence = Result(simResult.playedMoves, simResult.score).some))
        case Some(bestSequence) if ord.gt(simResult.score, bestSequence.score) =>
          game.update(_.copy(gameState = nextState, bestSequence = Result(simResult.playedMoves, simResult.score).some))
        case Some(bestSequence) =>
          // If none of the moves improve on the best sequence, the move of the best sequence is played
          val i        = bestSequence.moves.length - 1 - currentState.playedMoves.length
          val nextMove = bestSequence.moves(i)
          game.update(_.copy(gameState = currentState, bestSequence = bestSequence.some)) *> game.applyMove(nextMove)
      }
      bestTotal <- game.bestTotal
      _ <- bestTotal match {
        case None =>
          game.update(_.copy(bestTotal = Result(simResult.playedMoves, simResult.score).some)) *>
            game.log(show"""Improved sequence found:\n$simResult\n""")
        case Some(best) if ord.gt(simResult.score, best.score) =>
          game.update(_.copy(bestTotal = Result(simResult.playedMoves, simResult.score).some)) *>
            game.log(show"""Improved sequence found:\n$simResult\n""")
        case _ =>
          Monad[F].pure(())
      }
    } yield ()

  def nestedMonteCarlo[F[_]: Monad, Move, Position, Score](level: Int, game: Game[F, Move, Position, Score])(
      implicit ord: Ordering[Score],
      showResult: Show[GameState[Move, Position, Score]]): F[Unit] = {
    val playMoveWithBestSimulationResult = for {
      legalMoves          <- game.legalMoves
      currentState        <- game.gameState
      currentBestSequence <- game.bestSequence
      isTerminalPosition <- legalMoves match {
        case Nil => Monad[F].pure(true)
        case moves =>
          moves
            .traverse { move =>
              for {
                nextState <- game.update(_.copy(gameState = currentState, bestSequence = None)) *> game.applyMove(move) *> game.gameState
                simResult <- if (level <= 1) {
                  game.simulation *> game.gameState
                } else {
                  nestedMonteCarlo(level - 1, game) *> game.gameState
                }
              } yield (simResult, nextState)
            }
            .map(_.maxBy(_._1.score))
            .flatMap {
              case (bestSimResult, nextState) =>
                updateSearchState(game, currentState, nextState, currentBestSequence, bestSimResult).as(false)
            }
      }
    } yield isTerminalPosition

    playMoveWithBestSimulationResult.iterateUntil(isTerminalPosition => isTerminalPosition).void
  }
}

object Main extends IOApp {
  private val game  = data.Games.board(7)
  private val score = SameGame.score(game)
  private val s     = SearchState(Seed(234924L), GameState(List.empty[samegame.Position], score, game), None, None)

  private implicit val showResult: Show[GameState[samegame.Position, samegame.Game, Int]] = Interpreters.showResult

  private def putStrLn[T: Show](t: T): IO[Unit] = IO(println(show"$t"))

  val resultState: IO[Unit] = {
    val interpreter = Interpreters.withState()
    for {
      result <- Search.nestedMonteCarlo(3, interpreter).runS(s)
      _      <- putStrLn(result.gameState)
    } yield ()
  }

  val resultIORef: IO[Unit] = for {
    interpreter <- Interpreters.withIORef(s)
    result      <- Search.nestedMonteCarlo(2, interpreter) *> interpreter.gameState
    _           <- putStrLn(result)
  } yield ()

  def run(args: List[String]): IO[ExitCode] =
    // resultIORef.as(ExitCode.Success)
    resultState.as(ExitCode.Success)
}
