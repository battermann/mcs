package mcs.util

import mcs.samegame._
import scala.annotation.tailrec

object SearchStrategies {
  // some impure code to try stuff

  type Result    = mcs.Result[Position, Int]
  type GameState = mcs.GameState[Position, Game, Int]

  @tailrec
  def tabuColor(gameState: GameState): GameState = {
    val tb    = SameGame.predominantColor(gameState.position)
    val moves = SameGame.legalMoves(gameState.position)
    moves.partition(m => SameGame.color(gameState.position, m) == tb) match {
      case (Nil, Nil) => gameState
      case (tabuMoves, Nil) =>
        val i    = scala.util.Random.nextInt(tabuMoves.length)
        val move = tabuMoves(i)
        val gs   = SameGame.applyMove(move, gameState.position)
        tabuColor(mcs.GameState(move :: gameState.playedMoves, SameGame.score(gs), gs))
      case (_, ms) =>
        val i    = scala.util.Random.nextInt(ms.length)
        val move = ms(i)
        val gs   = SameGame.applyMove(move, gameState.position)
        tabuColor(mcs.GameState(move :: gameState.playedMoves, SameGame.score(gs), gs))
    }
  }

  @tailrec
  def random(gameState: GameState): GameState = {
    val moves = SameGame.legalMoves(gameState.position)
    println(s"move: ${gameState.playedMoves.length}")
    println(s"child nodes: ${moves.length}")
    moves match {
      case Nil => gameState
      case _ =>
        val i    = scala.util.Random.nextInt(moves.length)
        val move = moves(i)
        val gs   = SameGame.applyMove(move, gameState.position)
        random(mcs.GameState(move :: gameState.playedMoves, SameGame.score(gs), gs))
    }
  }

  private val (position, _) = mcs.data.Games.jsGames10
  private val score         = SameGame.score(position)
  private val gameState     = mcs.GameState(playedMoves = List.empty[Position], score = score, position = position)

  random(gameState)
}
