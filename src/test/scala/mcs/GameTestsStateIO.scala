package mcs

import mcs.Interpreters.{BoardPosition, Move, SearchState, StateIO}
import mcs.Prng.Seed
import mcs.samegame._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class GameTestsStateIO extends PropSpec with PropertyChecks with Matchers {
  implicit val game: Game[StateIO, Move, BoardPosition, Int] = Interpreters.gameInterpreterStateT

  def colEmpty(size: Int): List[CellState] = List.fill(size)(Empty)

  def colNonEmpty(size: Int): Gen[List[CellState]] =
    for {
      numFilled <- choose(1, size)
      filled    <- listOfN(numFilled, choose(0, 5).map(c => Filled(Color(c))))
    } yield filled ++ colEmpty(size - filled.length)

  def searchState(min: Int, max: Int): Gen[SearchState[Seed]] =
    for {
      size      <- choose(min, max)
      numFilled <- choose(min, size)
      nonEmpty  <- listOfN(numFilled, colNonEmpty(size)).map(_.map(Column(_)))
      empty     <- listOfN(size - numFilled, const(colEmpty(size))).map(_.map(Column(_)))
      score     <- choose(0, 1452)
      seed      <- Arbitrary.arbitrary[Long]
    } yield {
      val gameSate = GameState(
        playedMoves = List.empty[Position],
        position = SameGame.evaluateGameState(Board(nonEmpty ++ empty), score),
        score = score
      )
      SearchState(seed = Seed(seed), gameState = gameSate, bestSequence = None)
    }

  def move(boardSize: Int): Gen[Move] =
    for {
      col <- choose(0, boardSize)
      row <- choose(0, boardSize)
    } yield Position(col, row)

  property("simulation is terminal") {
    forAll(searchState(4, 8)) { st =>
      Game.laws
        .simulationIsTerminal[StateIO, Move, BoardPosition, Int]()
        .runA(st)
        .unsafeRunSync()
    }
  }

  property("legal move modifies game state") {
    forAll(searchState(4, 8), move(8)) {
      case (st, m) =>
        Game.laws
          .legalMoveModifiesGameState[StateIO, Move, BoardPosition, Int](m)
          .runA(st)
          .unsafeRunSync()
    }
  }
}
