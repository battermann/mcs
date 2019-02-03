package mcs

import cats.effect.concurrent.Ref
import cats.implicits._
import cats.{Eq, Monad, Parallel, Show}

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

    bestTotal
      .modify {
        case None =>
          val betterSequence = Result(simResult.playedMoves, simResult.score)
          (betterSequence.some, betterSequence.some)
        case Some(best) =>
          if (ord.gt(simResult.score, best.score)) {
            val betterSequence = Result(simResult.playedMoves, simResult.score)
            (betterSequence.some, betterSequence.some)
          } else {
            (best.some, None)
          }
      }
      .flatMap {
        case None         => Monad[F].pure(())
        case Some(better) => Logger[F].log(better)
      }
      .as(nextSearchState)
  }

  private def nestedSearch[F[_]: Monad: Logger, G[_], Move: Eq, Position, Score](searchState: SearchState[Move, Position, Score],
                                                                                 bestTotal: Ref[F, Option[Result[Move, Score]]],
                                                                                 numLevels: Int,
                                                                                 level: Int)(
      implicit game: Game[F, Move, Position, Score],
      par: Parallel[F, G],
      ord: Ordering[Score],
      showGameState: Show[GameState[Move, Position, Score]],
      showResult: Show[Result[Move, Score]]): F[SearchState[Move, Position, Score]] = {
    val legalMoves = game.legalMoves(searchState.gameState)
    for {
      _ <- if (level == numLevels) Logger[F].log(searchState.gameState) else Monad[F].pure(())
      result <- if (legalMoves.isEmpty) {
        Monad[F].pure(searchState)
      } else {
        if (level == 1) {
          legalMoves
            .traverse { move =>
              val nextState        = game.applyMove(searchState.gameState, move)
              val simulationResult = game.simulation(nextState)
              simulationResult.map((_, nextState))
            }
            .map(_.maxBy(_._1.score))
            .flatMap {
              case (simulationResult, nextState) =>
                chooseNextMove[F, Move, Position, Score](bestTotal, searchState.gameState, nextState, searchState.bestSequence, simulationResult)
            }
            .flatMap(st => nestedSearch[F, G, Move, Position, Score](st, bestTotal, numLevels, level))
        } else if (level == 2) {
          legalMoves
            .parTraverse { move =>
              val nextState    = game.applyMove(searchState.gameState, move)
              val bestSequence = searchState.bestSequence.filter(x => game.isPrefixOf(nextState.playedMoves)(x.moves))
              val simulationResult =
                nestedSearch[F, G, Move, Position, Score](SearchState(nextState, bestSequence), bestTotal, numLevels, 1)
                  .map(_.gameState)
              simulationResult.map((_, nextState))
            }
            .map(_.maxBy(_._1.score))
            .flatMap {
              case (simulationResult, nextState) =>
                chooseNextMove[F, Move, Position, Score](bestTotal, searchState.gameState, nextState, searchState.bestSequence, simulationResult)
            }
            .flatMap(st => nestedSearch[F, G, Move, Position, Score](st, bestTotal, numLevels, level))
        } else {
          legalMoves
            .traverse { move =>
              val nextState    = game.applyMove(searchState.gameState, move)
              val bestSequence = searchState.bestSequence.filter(x => game.isPrefixOf(nextState.playedMoves)(x.moves))
              val simulationResult =
                nestedSearch[F, G, Move, Position, Score](SearchState(nextState, bestSequence), bestTotal, numLevels, level - 1)
                  .map(_.gameState)
              simulationResult.map((_, nextState))
            }
            .map(_.maxBy(_._1.score))
            .flatMap {
              case (simulationResult, nextState) =>
                chooseNextMove[F, Move, Position, Score](bestTotal, searchState.gameState, nextState, searchState.bestSequence, simulationResult)
            }
            .flatMap(st => nestedSearch[F, G, Move, Position, Score](st, bestTotal, numLevels, level))
        }
      }
    } yield result
  }

  def nestedMonteCarlo[F[_]: Monad: Logger, G[_], Move: Eq, Position, Score](searchState: SearchState[Move, Position, Score],
                                                                             bestTotal: Ref[F, Option[Result[Move, Score]]],
                                                                             level: Int)(
      implicit game: Game[F, Move, Position, Score],
      par: Parallel[F, G],
      ord: Ordering[Score],
      showGameState: Show[GameState[Move, Position, Score]],
      showResult: Show[Result[Move, Score]]): F[SearchState[Move, Position, Score]] =
    nestedSearch[F, G, Move, Position, Score](searchState, bestTotal, level, level)
}
