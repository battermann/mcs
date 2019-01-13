package mcs.data

import mcs.samegame._

object Games {
  private def jsGames01Raw(): List[List[Int]] = List(
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
  )

  private def jsGames10Raw(): List[List[Int]] = List(
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
  )

  private def mkGame(xss: List[List[Int]]): Game = SameGame.evaluateGameState(Board(xss.map(c => Column(c.map(s => Filled(Color(s)))))), 0)

  val jsGames01: Game = mkGame(jsGames01Raw())
  val jsGames10: Game = mkGame(jsGames10Raw())

  val game1: Game = SameGame.evaluateGameState(Board(board15x15), 0)

  def board(size: Int): Game = SameGame.evaluateGameState(Board(board15x15.take(size).map(col => Column(col.cells.take(size)))), 0)

  def board15x15: List[Column] =
    List(
      Column(
        List(
          Filled(Color(3)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(2)),
          Filled(Color(1)),
          Filled(Color(2)),
          Filled(Color(2)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(3)),
          Filled(Color(4)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(3)),
          Filled(Color(1))
        )),
      Column(
        List(
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(2)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(3)),
          Filled(Color(3)),
          Filled(Color(1)),
          Filled(Color(0)),
          Filled(Color(1)),
          Filled(Color(1))
        )),
      Column(
        List(
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(3)),
          Filled(Color(1)),
          Filled(Color(2)),
          Filled(Color(0)),
          Filled(Color(2)),
          Filled(Color(2)),
          Filled(Color(1)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(2)),
          Filled(Color(0)),
          Filled(Color(1))
        )),
      Column(
        List(
          Filled(Color(1)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(3)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(1)),
          Filled(Color(3)),
          Filled(Color(3)),
          Filled(Color(0)),
          Filled(Color(2))
        )),
      Column(
        List(
          Filled(Color(1)),
          Filled(Color(0)),
          Filled(Color(2)),
          Filled(Color(3)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(0)),
          Filled(Color(1)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(3))
        )),
      Column(
        List(
          Filled(Color(0)),
          Filled(Color(1)),
          Filled(Color(1)),
          Filled(Color(1)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(2))
        )),
      Column(
        List(
          Filled(Color(1)),
          Filled(Color(3)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(3)),
          Filled(Color(0)),
          Filled(Color(2)),
          Filled(Color(0)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(0))
        )),
      Column(
        List(
          Filled(Color(1)),
          Filled(Color(2)),
          Filled(Color(4)),
          Filled(Color(0)),
          Filled(Color(2)),
          Filled(Color(2)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(4)),
          Filled(Color(2)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(1))
        )),
      Column(
        List(
          Filled(Color(1)),
          Filled(Color(1)),
          Filled(Color(3)),
          Filled(Color(3)),
          Filled(Color(4)),
          Filled(Color(3)),
          Filled(Color(3)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(2)),
          Filled(Color(3)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(4))
        )),
      Column(
        List(
          Filled(Color(0)),
          Filled(Color(2)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(3)),
          Filled(Color(4)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(2)),
          Filled(Color(1)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(4))
        )),
      Column(
        List(
          Filled(Color(1)),
          Filled(Color(1)),
          Filled(Color(1)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(2)),
          Filled(Color(0)),
          Filled(Color(1)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(4)),
          Filled(Color(4))
        )),
      Column(
        List(
          Filled(Color(4)),
          Filled(Color(0)),
          Filled(Color(3)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(2)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(3)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(2)),
          Filled(Color(2))
        )),
      Column(
        List(
          Filled(Color(3)),
          Filled(Color(4)),
          Filled(Color(2)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(4)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(3)),
          Filled(Color(3)),
          Filled(Color(4))
        )),
      Column(
        List(
          Filled(Color(4)),
          Filled(Color(2)),
          Filled(Color(4)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(4)),
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(0))
        )),
      Column(
        List(
          Filled(Color(0)),
          Filled(Color(0)),
          Filled(Color(1)),
          Filled(Color(3)),
          Filled(Color(1)),
          Filled(Color(0)),
          Filled(Color(3)),
          Filled(Color(2)),
          Filled(Color(2)),
          Filled(Color(1)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(4)),
          Filled(Color(2)),
          Filled(Color(1))
        ))
    )
}
