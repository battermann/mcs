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

trait Game[F[_], Move, Position, Score, Seed] {
  def applyMove(move: Move): F[Unit]
  def simulation: F[Unit]
  def updateGameState(gameState: GameState[Move, Position, Score]): F[Unit]
  def updateBestSequence(bestSequence: Option[Result[Move, Score]]): F[Unit]

  def legalMoves: F[List[Move]]
  def gameState: F[GameState[Move, Position, Score]]
  def bestSequence: F[Option[Result[Move, Score]]]
  def isPrefixOf(gameState: GameState[Move, Position, Score])(result: Result[Move, Score]): Boolean
  def next(gameState: GameState[Move, Position, Score], result: Result[Move, Score]): Option[Move]
}

object Game {
  def apply[F[_], Move, Position, Score, Seed]()(implicit ev: Game[F, Move, Position, Score, Seed]): Game[F, Move, Position, Score, Seed] = ev

  import cats.Monad
  import cats.implicits._

  object laws {
    def simulationIsTerminal[F[_]: Monad, Move, Position, Score, Seed](ev: Game[F, Move, Position, Score, Seed]): F[Boolean] = {
      for {
        _          <- ev.simulation.as(true)
        legalMoves <- ev.legalMoves
      } yield legalMoves.isEmpty
    }
  }
}

trait Logger[F[_]] {
  def log[T: Show](t: T): F[Unit]
}

object Logger {
  def apply[F[_]]()(implicit ev: Logger[F]): Logger[F] = ev
}
