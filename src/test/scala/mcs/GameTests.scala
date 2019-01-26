package mcs

import mcs.Interpreters.SearchState
import mcs.Prng.Seed
import mcs.samegame._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class GameTests extends PropSpec with PropertyChecks with Matchers {
  val game = Interpreters.gameInterpreterStateT

  def colEmpty(size: Int): List[CellState] = List.fill(size)(Empty)

  def colNonEmpty(size: Int): Gen[List[CellState]] =
    for {
      numFilled <- choose(1, size)
      filled    <- listOfN(numFilled, choose(0, 5).map(c => Filled(Color(c))))
    } yield filled ++ colEmpty(size - filled.length)

  val searchState: Gen[SearchState[Seed]] =
    for {
      size      <- choose(4, 8)
      numFilled <- choose(0, size)
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

  property("simulation is terminal") {
    forAll(searchState) { st =>
      Game.laws.simulationIsTerminal(game).runA(st).unsafeRunSync()
    }
  }
}
