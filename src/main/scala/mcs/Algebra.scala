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
    // `bestTotal` is not driving the search.
    // It just tracks the best overall result found so that it can be logged e.g.
    // When the search terminates `bestTotal` and `gameState` should represent the same sequence of moves.
    bestTotal: Option[Result[Move, Score]]
)

trait Game[F[_], Move, Position, Score, Seed] {
  type S = SearchState[Move, Position, Score, Seed]

  def applyMove(move: Move): F[Unit]
  def update(f: S => S): F[Unit]
  def simulation: F[Unit]

  def legalMoves: F[List[Move]]
  def rndInt(bound: Int): F[Int]
  def gameState: F[GameState[Move, Position, Score]]
  def bestSequence: F[Option[Result[Move, Score]]]
  def bestTotal: F[Option[Result[Move, Score]]]
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
