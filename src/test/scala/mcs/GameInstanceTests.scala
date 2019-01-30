package mcs

import cats.effect.IO
import mcs.Interpreters._
import mcs.samegame._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class GameInstanceTests extends PropSpec with PropertyChecks with Matchers {
  def colEmpty(size: Int): List[CellState] = List.fill(size)(Empty)

  def colNonEmpty(size: Int): Gen[List[CellState]] =
    for {
      numFilled <- choose(1, size)
      filled    <- listOfN(numFilled, choose(0, 5).map(c => Filled(Color(c))))
    } yield filled ++ colEmpty(size - filled.length)

  def gameState(min: Int, max: Int): Gen[GameState[Move, BoardPosition, Int]] =
    for {
      size      <- choose(min, max)
      numFilled <- choose(min, size)
      nonEmpty  <- listOfN(numFilled, colNonEmpty(size)).map(_.map(Column(_)))
      empty     <- listOfN(size - numFilled, const(colEmpty(size))).map(_.map(Column(_)))
      score     <- choose(0, 1452)
    } yield
      GameState(
        playedMoves = List.empty[Position],
        position = SameGame.evaluateGameState(Board(nonEmpty ++ empty), score),
        score = score
      )

  def move(boardSize: Int): Gen[Move] =
    for {
      col <- choose(0, boardSize)
      row <- choose(0, boardSize)
    } yield Position(col, row)

  property("simulation is terminal") {
    forAll(gameState(4, 8)) { gs =>
      Game.laws.simulationIsTerminal[IO, Move, BoardPosition, Int](gs).unsafeRunSync()
    }
  }

  property("legal move modifies game state") {
    forAll(gameState(4, 8), move(8)) {
      case (gs, m) =>
        Game.laws.legalMoveModifiesGameState[IO, Move, BoardPosition, Int](gs, m)
    }
  }
}
