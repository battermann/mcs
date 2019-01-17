package mcs

import cats.implicits._
import cats.{Eq, Monad, Show}

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
      bestTotal <- game.bestTotal
      _ <- bestTotal match {
        case None =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          game.updateBestTotal(betterSequence) *> Logger[F].log(betterSequence)
        case Some(best) if ord.gt(simResult.score, best.score) =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          game.updateBestTotal(betterSequence) *> Logger[F].log(betterSequence)
        case _ =>
          Monad[F].pure(())
      }
    } yield ()

  private def nested[F[_]: Monad: Logger, Move: Eq, Position, Score, Seed](levels: Int, level: Int, game: Game[F, Move, Position, Score, Seed])(
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
                nextState <- game.updateGameState(currentState) *> game.applyMove(move) *> game.gameState
                _         <- game.updateBestSequence(currentBestSequence.filter(game.isPrefixOf(nextState)))
                simResult <- if (level <= 1) { game.simulation *> game.gameState } else { nested(levels, level - 1, game) *> game.gameState }
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

  def nestedMonteCarlo[F[_]: Monad: Logger, Move: Eq, Position, Score, Seed](level: Int, game: Game[F, Move, Position, Score, Seed])(
      implicit ord: Ordering[Score],
      showGameState: Show[GameState[Move, Position, Score]],
      showResult: Show[Result[Move, Score]]): F[Unit] =
    nested(level, level, game)
}
