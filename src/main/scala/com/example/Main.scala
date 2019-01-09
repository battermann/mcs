package com.example

import cats.data._
import com.example.Prng.Seed
import cats.implicits._
import com.example.samegame.SameGame

final case class GameState[Move, Position, Score](
    playedMoves: List[Move],
    score: Score,
    position: Position,
)

final case class Result[Move, Score](
    moves: List[Move],
    score: Score
)

final case class SearchState[Move, Position, Score](
    seed: Seed,
    gameState: GameState[Move, Position, Score],
    bestResult: Option[Result[Move, Score]]
)

trait Game[Move, Position, Score] {
  type GS = GameState[Move, Position, Score]
  type S  = SearchState[Move, Position, Score]

  def applyMove(move: Move): State[S, Unit]
  def legalMoves: State[S, List[Move]]
  def gameState: State[S, GS] = State.inspect[S, GS](_.gameState)

  def rndInt(bound: Int): State[S, Int] = State[S, Int] { searchState =>
    val (nextSeed, i) = Prng.nextInt(bound).run(searchState.seed).value
    (searchState.copy(seed = nextSeed), i)
  }

  def rndSimulation(): State[S, Unit] = {
    val playRndLegalMove = for {
      moves <- legalMoves
      isTerminalPosition <- moves match {
        case Nil => State.pure[S, Boolean](true)
        case ms  => rndInt(ms.length).flatMap(i => applyMove(ms(i))).as(false)
      }
    } yield isTerminalPosition

    playRndLegalMove.iterateUntil(isTerminalPosition => isTerminalPosition).void
  }
}

object Search {
  def simpleMonteCarlo[Move, Position, Score](game: Game[Move, Position, Score])(
      implicit ord: Ordering[Score]): State[SearchState[Move, Position, Score], Unit] = {
    type S = SearchState[Move, Position, Score]

    val playMoveWithBestSimulationResult = for {
      moves        <- game.legalMoves
      currentState <- game.gameState
      isTerminalPosition <- moves match {
        case Nil => State.pure[S, Boolean](true)
        case ms =>
          ms.traverse { m =>
              for {
                _         <- State.modify[S](_.copy(gameState = currentState))
                _         <- game.applyMove(m)
                nextState <- game.gameState
                _         <- game.rndSimulation()
                simResult <- game.gameState
              } yield (simResult, nextState)
            }
            .map(_.maxBy(_._1.score))
            .flatMap {
              case (simResult, nextState) =>
                State
                  .modify[S] {
                    case st @ SearchState(_, _, None) =>
                      st.copy(gameState = nextState, bestResult = Result(simResult.playedMoves, simResult.score).some)
                    case st @ SearchState(_, _, Some(result)) =>
                      if (ord.gt(simResult.score, result.score)) {
                        st.copy(gameState = nextState, bestResult = Result(simResult.playedMoves, simResult.score).some)
                      } else {
                        st.copy(gameState = nextState, bestResult = result.some)
                      }
                  }
                  .as(false)
            }
      }
    } yield isTerminalPosition

    playMoveWithBestSimulationResult.iterateUntil(b => b).void
  }
}

final class SG extends Game[samegame.Position, samegame.Game, Int] {
  def applyMove(move: samegame.Position): State[S, Unit] =
    State.modify[S] { searchState =>
      val nextPosition = SameGame.applyMove(move, searchState.gameState.position)
      val gameState = searchState.gameState.copy(
        position = nextPosition,
        score = SameGame.score(nextPosition),
        playedMoves = move :: searchState.gameState.playedMoves
      )
      searchState.copy(gameState = gameState)
    }

  def legalMoves: State[S, List[samegame.Position]] =
    State.inspect[S, List[samegame.Position]] { searchState =>
      SameGame.legalMoves(searchState.gameState.position)
    }
}

object SG {
  def apply(): SG = new SG()
}

object Main extends App {

  val game  = data.Games.game1
  val score = SameGame.score(game)
  val s     = SearchState(Seed(5L), GameState(List.empty[samegame.Position], score, game), None)

  val Some(r) = Search.simpleMonteCarlo(SG()).runS(s).value.bestResult

  println(r.moves.reverse.map(p => s"(${p.col}, ${p.row})").mkString("[", ", ", "]"))
  println(s"Score: ${r.score}")
}
