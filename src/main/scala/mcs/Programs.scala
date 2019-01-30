package mcs

import cats.effect.concurrent.Ref
import cats.implicits._
import cats.{Eq, Monad, Show}

object Programs {

  final case class SearchState[Move, Position, Score](
      gameState: GameState[Move, Position, Score],
      bestSequence: Option[Result[Move, Score]],
  )

  def chooseNextMove[F[_]: Monad: Logger, Move, Position, Score](bestTotal: Ref[F, Option[Result[Move, Score]]],
                                                                 currentState: GameState[Move, Position, Score],
                                                                 nextState: GameState[Move, Position, Score],
                                                                 currentBestSequence: Option[Result[Move, Score]],
                                                                 simResult: GameState[Move, Position, Score])(
      implicit game: Game[F, Move, Position, Score],
      ord: Ordering[Score],
      showResult: Show[Result[Move, Score]]): F[SearchState[Move, Position, Score]] = {
    val nextSearchState = currentBestSequence match {
      case None =>
        SearchState(nextState, Result(simResult.playedMoves, simResult.score).some)
      case Some(currentBest) =>
        if (ord.gteq(simResult.score, currentBest.score)) {
          SearchState(nextState, Result(simResult.playedMoves, simResult.score).some)
        } else {
          // If none of the moves improve or equals best sequence, the move of the best sequence is played
          game
            .next(currentState.playedMoves, currentBest.moves)
            .fold(SearchState(nextState, Result(simResult.playedMoves, simResult.score).some))(m =>
              SearchState(game.applyMove(currentState, m), currentBest.some))
        }
    }

    bestTotal.get
      .flatMap {
        case None =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          bestTotal.set(betterSequence.some) *> Logger[F].log(betterSequence)
        case Some(best) if ord.gt(simResult.score, best.score) =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          bestTotal.set(betterSequence.some) *> Logger[F].log(betterSequence)
        case _ =>
          Monad[F].pure(())
      }
      .as(nextSearchState)
  }

  private def nestedSearch[F[_]: Monad: Logger, Move: Eq, Position, Score](searchState: SearchState[Move, Position, Score],
                                                                           bestTotal: Ref[F, Option[Result[Move, Score]]],
                                                                           numLevels: Int,
                                                                           level: Int)(
      implicit game: Game[F, Move, Position, Score],
      ord: Ordering[Score],
      showGameState: Show[GameState[Move, Position, Score]],
      showResult: Show[Result[Move, Score]]): F[SearchState[Move, Position, Score]] = {
    val legalMoves = game.legalMoves(searchState.gameState)
    (if (level == numLevels) Logger[F].log(searchState.gameState) else Monad[F].pure(()))
      .flatMap(_ =>
        legalMoves match {
          case Nil => Monad[F].pure(searchState)
          case moves =>
            moves
              .traverse { move =>
                val nextState    = game.applyMove(searchState.gameState, move)
                val bestSequence = searchState.bestSequence.filter(x => game.isPrefixOf(nextState.playedMoves)(x.moves))
                val simulationResult =
                  if (level == 1)
                    game.simulation(nextState)
                  else
                    nestedSearch[F, Move, Position, Score](SearchState(nextState, bestSequence), bestTotal, numLevels, level - 1)
                      .map(_.gameState)
                simulationResult.map((_, nextState))
              }
              .map(_.maxBy(_._1.score))
              .flatMap {
                case (simulationResult, nextState) =>
                  chooseNextMove[F, Move, Position, Score](bestTotal, searchState.gameState, nextState, searchState.bestSequence, simulationResult)
              }
              .flatMap(st => nestedSearch[F, Move, Position, Score](st, bestTotal, numLevels, level))
      })
  }

  def nestedMonteCarlo[F[_]: Monad: Logger, Move: Eq, Position, Score](searchState: SearchState[Move, Position, Score],
                                                                        bestTotal: Ref[F, Option[Result[Move, Score]]],
                                                                        level: Int)(
      implicit game: Game[F, Move, Position, Score],
      ord: Ordering[Score],
      showGameState: Show[GameState[Move, Position, Score]],
      showResult: Show[Result[Move, Score]]): F[SearchState[Move, Position, Score]] =
    nestedSearch[F, Move, Position, Score](searchState, bestTotal, level, level)
}
