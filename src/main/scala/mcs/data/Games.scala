package mcs.data

import mcs.samegame._
import cats.implicits._
import mcs.Result

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
         Position(8, 13),
         Position(11, 13),
         Position(12, 3),
         Position(12, 0),
         Position(9, 10),
         Position(13, 12),
         Position(12, 6),
         Position(8, 2),
         Position(14, 4),
         Position(1, 1),
         Position(0, 8),
         Position(5, 1),
         Position(11, 9),
         Position(12, 1),
         Position(5, 9),
         Position(6, 11),
         Position(4, 0),
         Position(1, 4),
         Position(3, 3),
         Position(7, 6),
         Position(12, 1),
         Position(5, 6),
         Position(4, 2),
         Position(12, 2),
         Position(2, 5),
         Position(12, 1),
         Position(9, 0),
         Position(5, 0),
         Position(2, 6),
         Position(0, 2),
         Position(0, 7),
         Position(1, 7),
         Position(7, 5),
         Position(2, 7),
         Position(1, 3),
         Position(11, 1),
         Position(5, 2),
         Position(7, 1),
         Position(4, 0),
         Position(6, 1),
         Position(7, 2),
         Position(7, 1),
         Position(7, 0),
         Position(6, 0),
         Position(11, 1),
         Position(0, 1),
         Position(10, 9),
         Position(13, 8),
         Position(0, 0),
         Position(3, 2),
         Position(11, 2),
         Position(9, 0),
         Position(5, 0),
         Position(3, 0),
         Position(7, 1),
         Position(8, 5),
         Position(6, 3),
         Position(7, 0),
         Position(11, 3),
         Position(10, 0),
         Position(7, 1),
         Position(1, 0),
         Position(0, 2),
         Position(0, 1),
         Position(7, 1),
         Position(5, 1),
         Position(0, 0),
         Position(0, 0),
         Position(1, 0),
         Position(0, 0)
       ).reverse,
       2197
     ).some)

  val jsGames01: (Game, Option[Result[Position, Int]]) = (SameGame.apply(jsGames01Raw()._1), jsGames01Raw()._2)
  val jsGames10: (Game, Option[Result[Position, Int]]) = (SameGame.apply(jsGames10Raw()._1), jsGames10Raw()._2)

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
