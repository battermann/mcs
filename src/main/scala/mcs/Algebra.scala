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

trait Game[F[_], Move, Position, Score] {
  def applyMove(move: Move): F[Unit]
  def simulation: F[Unit]
  def updateGameState(gameState: GameState[Move, Position, Score]): F[Unit]
  def updateBestSequence(bestSequence: Result[Move, Score]): F[Unit]

  def legalMoves: F[List[Move]]
  def gameState: F[GameState[Move, Position, Score]]
  def bestSequence: F[Option[Result[Move, Score]]]
  def isPrefixOf(gameState: GameState[Move, Position, Score])(result: Result[Move, Score]): Boolean
  def next(gameState: GameState[Move, Position, Score], result: Result[Move, Score]): Option[Move]
}

object Game {
  object laws {
    import cats.Monad
    import cats.implicits._

    def simulationIsTerminal[F[_]: Monad, Move, BoardPosition, Score]()(implicit ev: Game[F, Move, BoardPosition, Score]): F[Boolean] =
      ev.simulation *> ev.legalMoves map (_.isEmpty)

    def legalMoveModifiesGameState[F[_]: Monad, Move, BoardPosition, Score](m: Move)(implicit ev: Game[F, Move, BoardPosition, Score]): F[Boolean] =
      for {
        gameState <- ev.gameState
        moves     <- ev.legalMoves
        result <- if (!moves.contains(m))
          Monad[F].pure(true)
        else
          ev.applyMove(m) *> ev.gameState.map(gs => (gs.position != gameState.position) && (gs.playedMoves.length == gameState.playedMoves.length + 1))
      } yield result
  }
}

trait Logger[F[_]] {
  def log[T: Show](t: T): F[Unit]
}

object Logger {
  def apply[F[_]]()(implicit ev: Logger[F]): Logger[F] = ev
}
