package mcs.data

import mcs.samegame._
import cats.implicits._
import mcs.{GameState, Interpreters, Result}

object Games {
  private def jsGames01Raw(): (List[List[Int]], Option[Result[Position, Int]]) =
    (List(
       List(1, 1, 3, 2, 0, 0, 1, 1, 2, 0, 1, 2, 0, 3, 3),
       List(0, 0, 3, 2, 4, 3, 3, 0, 4, 4, 2, 3, 2, 3, 1),
       List(1, 3, 0, 3, 1, 3, 3, 0, 3, 4, 1, 4, 3, 2, 1),
       List(0, 2, 2, 1, 2, 4, 2, 4, 4, 3, 3, 0, 4, 0, 4),
       List(1, 1, 3, 0, 0, 2, 0, 0, 2, 0, 1, 2, 3, 4, 1),
       List(1, 4, 2, 4, 1, 3, 4, 3, 3, 3, 2, 3, 0, 4, 0),
       List(2, 4, 1, 0, 3, 0, 3, 1, 1, 4, 0, 0, 3, 1, 4),
       List(2, 4, 4, 1, 4, 0, 1, 2, 1, 2, 1, 2, 0, 3, 0),
       List(1, 4, 3, 2, 3, 2, 3, 1, 1, 2, 2, 4, 0, 1, 4),
       List(0, 0, 1, 4, 3, 1, 0, 0, 3, 2, 1, 4, 3, 2, 4),
       List(0, 4, 3, 1, 4, 2, 4, 4, 4, 0, 0, 4, 4, 0, 1),
       List(1, 2, 0, 3, 1, 3, 1, 1, 1, 2, 3, 3, 4, 0, 1),
       List(4, 1, 2, 3, 4, 4, 0, 3, 0, 3, 4, 0, 1, 4, 0),
       List(3, 3, 1, 0, 0, 0, 0, 3, 3, 4, 0, 2, 1, 0, 2),
       List(2, 4, 3, 1, 4, 1, 3, 1, 1, 0, 1, 3, 1, 4, 3)
     ),
     None)

  private def jsGames10Raw(): (List[List[Int]], Option[Result[Position, Int]]) =
    (List(
       List(0, 2, 3, 1, 2, 1, 3, 1, 0, 2, 2, 3, 1, 4, 0),
       List(3, 0, 0, 3, 3, 2, 4, 2, 0, 4, 0, 4, 2, 0, 1),
       List(1, 2, 2, 2, 4, 2, 3, 0, 0, 2, 0, 3, 4, 1, 3),
       List(1, 1, 2, 4, 2, 2, 3, 4, 2, 2, 3, 0, 3, 1, 3),
       List(0, 0, 1, 3, 1, 4, 1, 1, 2, 1, 2, 1, 0, 0, 4),
       List(1, 4, 4, 0, 0, 3, 1, 2, 3, 4, 0, 3, 2, 0, 3),
       List(4, 1, 3, 4, 1, 2, 0, 2, 4, 3, 2, 3, 0, 0, 4),
       List(1, 4, 4, 4, 2, 0, 4, 4, 3, 2, 3, 2, 2, 1, 3),
       List(4, 0, 1, 1, 3, 2, 4, 0, 2, 1, 3, 3, 3, 2, 2),
       List(1, 3, 2, 1, 2, 1, 2, 2, 3, 4, 3, 1, 4, 0, 4),
       List(1, 2, 2, 4, 4, 0, 1, 4, 0, 0, 0, 1, 3, 3, 4),
       List(1, 3, 1, 1, 0, 1, 0, 2, 3, 1, 1, 0, 1, 0, 0),
       List(0, 0, 1, 2, 2, 0, 0, 4, 4, 4, 1, 3, 2, 0, 3),
       List(4, 2, 3, 4, 4, 1, 1, 0, 3, 4, 1, 4, 2, 2, 2),
       List(2, 4, 1, 0, 3, 3, 3, 3, 4, 1, 1, 2, 3, 1, 1)
     ),
     Result(
       List(
         (0, 8),
         (2, 6),
         (13, 5),
         (4, 6),
         (12, 0),
         (13, 3),
         (9, 0),
         (11, 10),
         (12, 2),
         (11, 7),
         (9, 13),
         (10, 11),
         (4, 0),
         (10, 4),
         (4, 8),
         (5, 9),
         (14, 4),
         (4, 8),
         (13, 2),
         (7, 10),
         (2, 11),
         (14, 4),
         (3, 3),
         (5, 0),
         (6, 3),
         (10, 2),
         (5, 2),
         (1, 3),
         (11, 1),
         (1, 1),
         (11, 2),
         (0, 8),
         (2, 3),
         (7, 3),
         (5, 2),
         (5, 0),
         (6, 0),
         (6, 1),
         (14, 5),
         (10, 3),
         (7, 5),
         (1, 5),
         (1, 4),
         (14, 1),
         (5, 0),
         (4, 1),
         (9, 1),
         (11, 0),
         (10, 0),
         (7, 1),
         (1, 0),
         (0, 0),
         (0, 1),
         (0, 0),
         (0, 0),
         (3, 0),
         (0, 0),
         (0, 0),
         (0, 0),
         (0, 0)
       ).map(p => Position(p._1, p._2)).reverse,
       3215
     ).some)

  val jsGames01: (Game, Option[Result[Position, Int]]) = (SameGame.apply(jsGames01Raw()._1), jsGames01Raw()._2)
  val jsGames10: (Game, Option[Result[Position, Int]]) = (SameGame.apply(jsGames10Raw()._1), jsGames10Raw()._2)

  val foo: GameState[Position, Game, Int] = {
    val (position, b) = Games.jsGames10
    val score         = SameGame.score(position)
    val gameState     = mcs.GameState(playedMoves = List.empty[Position], score = score, position = position)

    val result = b.map { x =>
      x.moves.drop(20).foldRight(gameState) {
        case (m, state) =>
          Interpreters.game.applyMove(state, m)
      }
    }
    result.get
  }

  val game1: (Game, Option[Result[Position, Int]]) = (SameGame.apply(board15x15), None)

  def board(size: Int): (Game, Option[Result[Position, Int]]) =
    (SameGame.apply(board15x15.take(size).map(_.take(size))), None)

  def board15x15: List[List[Int]] =
    List(
      List(3, 0, 4, 2, 1, 2, 2, 0, 0, 3, 4, 3, 2, 3, 1),
      List(1, 4, 4, 2, 3, 2, 0, 0, 0, 3, 3, 1, 0, 1, 1),
      List(0, 4, 4, 3, 1, 2, 0, 2, 2, 1, 0, 0, 2, 0, 1),
      List(1, 3, 2, 1, 4, 1, 3, 4, 4, 1, 1, 3, 3, 0, 2),
      List(1, 0, 2, 3, 4, 1, 3, 2, 0, 1, 1, 4, 1, 4, 3),
      List(0, 1, 1, 1, 0, 4, 0, 0, 0, 0, 4, 0, 0, 4, 2),
      List(1, 3, 3, 2, 3, 0, 2, 0, 1, 4, 0, 0, 3, 2, 0),
      List(1, 2, 4, 0, 2, 2, 3, 2, 4, 2, 4, 4, 4, 1, 1),
      List(1, 1, 3, 3, 4, 3, 3, 0, 4, 1, 2, 3, 4, 1, 4),
      List(0, 2, 4, 1, 4, 3, 4, 3, 2, 2, 1, 1, 4, 4, 4),
      List(1, 1, 1, 0, 4, 4, 1, 4, 2, 0, 1, 3, 2, 4, 4),
      List(4, 0, 3, 3, 2, 2, 4, 4, 3, 4, 4, 4, 4, 2, 2),
      List(3, 4, 2, 3, 2, 4, 0, 0, 4, 4, 1, 4, 3, 3, 4),
      List(4, 2, 4, 3, 2, 3, 2, 0, 4, 0, 0, 4, 0, 0, 0),
      List(0, 0, 1, 3, 1, 0, 3, 2, 2, 1, 4, 4, 4, 2, 1)
    )

}
