package com.example

import cats.{Monad, Show}
import cats.implicits._

object Search {
  def updateSearchState[F[_]: Monad, Move, Position, Score, Seed](
      game: Game[F, Move, Position, Score, Seed],
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

  def nestedMonteCarlo[F[_]: Monad, Move, Position, Score, Seed](level: Int, game: Game[F, Move, Position, Score, Seed])(
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
