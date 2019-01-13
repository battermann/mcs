package mcs.samegame

final case class Position(col: Int, row: Int)

sealed trait Color
case object Green extends Color
case object Blue  extends Color
case object Red   extends Color
case object Brown extends Color
case object Gray  extends Color

sealed trait CellState
final case class Filled(color: Color) extends CellState
case object Empty                     extends CellState

final case class Cell(position: Position, state: CellState)
final case class Group(color: Color, positions: Set[Position])
final case class Column(cells: List[CellState])        extends AnyVal
final case class Board private (columns: List[Column]) extends AnyVal

sealed trait Game
final case class InProgress(board: Board, score: Int) extends Game
final case class Finished(board: Board, score: Int)   extends Game

object Color {
  def apply(n: Int): Color = {
    n % 5 match {
      case 0 => Green
      case 1 => Blue
      case 2 => Red
      case 3 => Brown
      case 4 => Gray
    }
  }
}

object Position {
  def left(pos: Position): Position = {
    Position(pos.col - 1, pos.row)
  }

  def right(pos: Position): Position = {
    Position(pos.col + 1, pos.row)
  }

  def up(pos: Position): Position = {
    Position(pos.col, pos.row + 1)
  }

  def down(pos: Position): Position = {
    Position(pos.col, pos.row - 1)
  }
}

object Column {
  implicit class CellMapper(column: Column) {
    def map(f: (CellState, Int) => CellState): Column = {
      Column(column.cells.zipWithIndex.map { case (cs, i) => f(cs, i) })
    }

    def shiftDown: Column = {
      val nonEmptyCells = column.cells
        .filter(!CellState.isEmpty(_))

      val diff = column.cells.length - nonEmptyCells.length

      Column(nonEmptyCells ++ List.fill(diff)(Empty))
    }
  }

  def empty(height: Int): Column = Column((1 to height).map(_ => Empty).toList)
}

object CellState {
  def isEmpty(cellState: CellState): Boolean = {
    cellState match {
      case Empty => true
      case _     => false
    }
  }
}

object Board {
  implicit class ColumnMapper(board: Board) {
    def map(f: (Column, Int) => Column): Board = {
      Board(board.columns.zipWithIndex.map { case (c, i) => f(c, i) })
    }

    def shiftLeft: Board = {
      val nonEmptyColumns = board.columns
        .filter(column => !CellState.isEmpty(column.cells.head))

      val diff   = board.columns.length - nonEmptyColumns.length
      val height = board.columns.head.cells.length
      Board(nonEmptyColumns ++ List.fill(diff)(Column.empty(height)))
    }
  }
}
