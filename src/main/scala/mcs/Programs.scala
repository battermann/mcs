package mcs

import cats.effect.concurrent.Ref
import cats.implicits._
import cats.{Eq, Monad, Show}

object Programs {
  private def chooseNextMove[F[_]: Monad: Logger, Move, Position, Score, Seed](bestTotal: Ref[F, Option[Result[Move, Score]]],
                                                                               currentState: GameState[Move, Position, Score],
                                                                               nextState: GameState[Move, Position, Score],
                                                                               currentBestSequence: Option[Result[Move, Score]],
                                                                               simResult: GameState[Move, Position, Score])(
      implicit game: Game[F, Move, Position, Score, Seed],
      ord: Ordering[Score],
      showResult: Show[Result[Move, Score]]): F[Unit] =
    for {
      _ <- currentBestSequence match {
        case None =>
          game.updateGameState(nextState) *> game.updateBestSequence(Result(simResult.playedMoves, simResult.score).some)
        case Some(currentBest) =>
          if (ord.gteq(simResult.score, currentBest.score)) {
            game.updateGameState(nextState) *> game.updateBestSequence(Result(simResult.playedMoves, simResult.score).some)
          } else {
            // If none of the moves improve or equals best sequence, the move of the best sequence is played
            game
              .next(currentState, currentBest)
              .fold(game.updateGameState(nextState) *> game.updateBestSequence(Result(simResult.playedMoves, simResult.score).some))(m =>
                game.updateGameState(currentState) *> game.updateBestSequence(currentBest.some) *> game.applyMove(m))
          }
      }
      maybeBestTotal <- bestTotal.get
      _ <- maybeBestTotal match {
        case None =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          bestTotal.set(betterSequence.some) *> Logger[F].log(betterSequence)
        case Some(best) if ord.gt(simResult.score, best.score) =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          bestTotal.set(betterSequence.some) *> Logger[F].log(betterSequence)
        case _ =>
          Monad[F].pure(())
      }
    } yield ()

  def nestedMonteCarlo[F[_]: Monad: Logger, Move: Eq, Position, Score, Seed](bestTotal: Ref[F, Option[Result[Move, Score]]], level: Int)(
      implicit game: Game[F, Move, Position, Score, Seed],
      ord: Ordering[Score],
      showGameState: Show[GameState[Move, Position, Score]],
      showResult: Show[Result[Move, Score]]): F[GameState[Move, Position, Score]] = {
    val playMoveWithBestSimulationResult = for {
      legalMoves          <- game.legalMoves
      currentState        <- game.gameState
      currentBestSequence <- game.bestSequence
      isTerminalPosition <- legalMoves match {
        case Nil => Monad[F].pure((true, currentState))
        case moves =>
          moves
            .traverse { move =>
              for {
                nextState <- game.updateGameState(currentState) *> game.applyMove(move) *> game.gameState
                _         <- game.updateBestSequence(currentBestSequence.filter(game.isPrefixOf(nextState)))
                simResult <- if (level <= 1) { game.simulation *> game.gameState } else {
                  nestedMonteCarlo[F, Move, Position, Score, Seed](bestTotal, level - 1) *> game.gameState
                }
              } yield (simResult, nextState)
            }
            .map(_.maxBy(_._1.score))
            .flatMap {
              case (bestSimResult, nextState) =>
                chooseNextMove[F, Move, Position, Score, Seed](bestTotal, currentState, nextState, currentBestSequence, bestSimResult).as((false, nextState))
            }
      }
    } yield isTerminalPosition

    playMoveWithBestSimulationResult.iterateUntil(result => result._1).map(_._2)
  }
}
