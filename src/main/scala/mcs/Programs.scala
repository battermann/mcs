package mcs

import cats.{Monad, Show}
import cats.implicits._

object Programs {
  private def chooseNextMove[F[_]: Monad: Logger, Move, Position, Score, Seed](
      game: Game[F, Move, Position, Score, Seed],
      currentState: GameState[Move, Position, Score],
      nextState: GameState[Move, Position, Score],
      currentBestSequence: Option[Result[Move, Score]],
      simResult: GameState[Move, Position, Score])(implicit ord: Ordering[Score], showResult: Show[Result[Move, Score]]): F[Unit] =
    for {
      _ <- currentBestSequence match {
        case None =>
          game.update(_.copy(gameState = nextState, bestSequence = Result(simResult.playedMoves, simResult.score).some))
        case Some(currentBest) if ord.gt(simResult.score, currentBest.score) =>
          game.update(_.copy(gameState = nextState, bestSequence = Result(simResult.playedMoves, simResult.score).some))
        case Some(currentBest) =>
          // If none of the moves improve on the best sequence, the move of the best sequence is played
          val i        = currentBest.moves.length - 1 - currentState.playedMoves.length
          val nextMove = currentBest.moves(i)
          game.update(_.copy(gameState = currentState, bestSequence = currentBest.some)) *> game.applyMove(nextMove)
      }
      bestTotal <- game.bestTotal
      _ <- bestTotal match {
        case None =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          game.update(_.copy(bestTotal = betterSequence.some)) *>
            Logger[F].log(betterSequence)
        case Some(best) if ord.gt(simResult.score, best.score) =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          game.update(_.copy(bestTotal = betterSequence.some)) *>
            Logger[F].log(betterSequence)
        case _ =>
          Monad[F].pure(())
      }
    } yield ()

  private def nested[F[_]: Monad: Logger, Move, Position, Score, Seed](levels: Int, level: Int, game: Game[F, Move, Position, Score, Seed])(
      implicit ord: Ordering[Score],
      showGameState: Show[GameState[Move, Position, Score]],
      showResult: Show[Result[Move, Score]]): F[Unit] = {
    val playMoveWithBestSimulationResult = for {
      legalMoves          <- game.legalMoves
      currentState        <- game.gameState
      _                   <- if (levels == level) Logger[F].log(currentState) else Monad[F].pure(())
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
                  nested(levels, level - 1, game) *> game.gameState
                }
              } yield (simResult, nextState)
            }
            .map(_.maxBy(_._1.score))
            .flatMap {
              case (bestSimResult, nextState) =>
                chooseNextMove(game, currentState, nextState, currentBestSequence, bestSimResult).as(false)
            }
      }
    } yield isTerminalPosition

    playMoveWithBestSimulationResult.iterateUntil(isTerminalPosition => isTerminalPosition).void
  }

  def nestedMonteCarlo[F[_]: Monad: Logger, Move, Position, Score, Seed](level: Int, game: Game[F, Move, Position, Score, Seed])(
      implicit ord: Ordering[Score],
      showGameState: Show[GameState[Move, Position, Score]],
      showResult: Show[Result[Move, Score]]): F[Unit] =
    nested(level, level, game)
}
