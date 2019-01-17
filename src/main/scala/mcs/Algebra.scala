package mcs

import cats.Show

final case class GameState[Move, Position, Score](
    playedMoves: List[Move],
    score: Score,
    position: Position,
)

final case class Result[Move, Score](
    moves: List[Move],
    score: Score
)

final case class SearchState[Move, Position, Score, Seed](
    seed: Seed,
    gameState: GameState[Move, Position, Score],
    bestSequence: Option[Result[Move, Score]],
    /** `bestTotal` is not driving the search. But tracks overall best result for logging. */
    bestTotal: Option[Result[Move, Score]]
)

trait Game[F[_], Move, Position, Score, Seed] {
  type S = SearchState[Move, Position, Score, Seed]

  def applyMove(move: Move): F[Unit]
  def simulation: F[Unit]
  def updateGameState(gameState: GameState[Move, Position, Score]): F[Unit]
  def updateBestTotal(bestTotal: Result[Move, Score]): F[Unit]
  def updateBestSequence(bestSequence: Option[Result[Move, Score]]): F[Unit]

  def legalMoves: F[List[Move]]
  def gameState: F[GameState[Move, Position, Score]]
  def bestSequence: F[Option[Result[Move, Score]]]
  def bestTotal: F[Option[Result[Move, Score]]]
  def isPrefixOf(gameState: GameState[Move, Position, Score])(result: Result[Move, Score]): Boolean
  def next(gameState: GameState[Move, Position, Score], result: Result[Move, Score]): Option[Move]
}

object Game {
  def apply[F[_], Move, Position, Score, Seed]()(implicit ev: Game[F, Move, Position, Score, Seed]): Game[F, Move, Position, Score, Seed] = ev
}

trait Logger[F[_]] {
  def log[T: Show](t: T): F[Unit]
}

object Logger {
  def apply[F[_]]()(implicit ev: Logger[F]): Logger[F] = ev
}
