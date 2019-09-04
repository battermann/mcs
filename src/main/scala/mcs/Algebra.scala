package mcs

import cats.Show

final case class Result[Move, Score](
  moves: List[Move],
  score: Score
)

final case class GameState[Move, Position, Score](
  playedMoves: List[Move],
  score: Score,
  position: Position,
)

trait Game[F[_], Move, Position, Score] {
  def applyMove(gameState: GameState[Move, Position, Score], move: Move): GameState[Move, Position, Score]
  def legalMoves(gameState: GameState[Move, Position, Score]): List[Move]
  def simulation(gameState: GameState[Move, Position, Score]): F[GameState[Move, Position, Score]]
  def isPrefixOf: List[Move] => List[Move] => Boolean
  def next(currentPath: List[Move], bestPath: List[Move]): Option[Move]
}

object Game {
  object laws {
    import cats.Monad
    import cats.implicits._

    def simulationIsTerminal[F[_]: Monad, Move, Position, Score](
      gameState: GameState[Move, Position, Score]
    )(implicit ev: Game[F, Move, Position, Score]): F[Boolean] =
      ev.simulation(gameState).map(ev.legalMoves).map(_.isEmpty)

    def legalMoveModifiesGameState[F[_]: Monad, Move, Position, Score](gameState: GameState[Move, Position, Score],
                                                                       move: Move)(implicit ev: Game[F, Move, Position, Score]): Boolean = {
      val legalMoves    = ev.legalMoves(gameState)
      val nextGameState = ev.applyMove(gameState, move)
      !legalMoves.contains(move) || (nextGameState.position != gameState.position && nextGameState.playedMoves.length == gameState.playedMoves.length + 1)
    }
  }
}

trait Logger[F[_]] {
  def log[T: Show](t: T): F[Unit]
}

object Logger {
  def apply[F[_]]()(implicit ev: Logger[F]): Logger[F] = ev
}
