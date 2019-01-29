package mcs.samegame

import Column.CellMapper
import Board.ColumnMapper
import cats.implicits._

object SameGame {
  private val bonus = 1000

  private def sqr(x: Int): Int = x * x

  private def calcScore(group: Group) = sqr(group.positions.size - 2)

  private def penalty(numberOfFilledCells: Int) = -sqr(numberOfFilledCells - 2)

  private def getCellState(board: Board, position: Position): CellState = {
    val width  = board.columns.length
    val height = board.columns.head.cells.length
    if (position.col >= 0 && position.col < width && position.row >= 0 && position.row < height) {
      board.columns(position.col).cells(position.row)
    } else {
      Empty
    }
  }

  private def findAdjacentWithSameColor(board: Board, position: Position): Set[Position] = {
    getCellState(board, position) match {
      case Filled(color) =>
        Set(
          Position.up(position),
          Position.right(position),
          Position.down(position),
          Position.left(position)
        ).map(p => (getCellState(board, p), p))
          .filter {
            case (Filled(c), _) => c == color
            case _              => false
          }
          .map(_._2)

      case Empty => Set()
    }
  }

  private def hasValidMoves(board: Board): Boolean = {
    board.columns.zipWithIndex
      .exists {
        case (column, colIndex) =>
          column.cells.zipWithIndex
            .exists {
              case (_, rowIndex) =>
                findAdjacentWithSameColor(board, Position(colIndex, rowIndex)).nonEmpty
            }
      }
  }

  private def filledCells(board: Board): Int = {
    board.columns
      .foldLeft(0)((total, column) =>
        column.cells.foldLeft(total)((count, cell) =>
          cell match {
            case Filled(_) => count + 1
            case Empty     => count
        }))
  }

  private def findGroup(board: Board, position: Position): Option[Group] = {
    def find(toSearch: Set[Position], group: Set[Position]): Set[Position] = {
      if (toSearch.isEmpty) {
        group
      } else {
        val head               = toSearch.head
        val cellsWithSameColor = findAdjacentWithSameColor(board, head)
        val cellsFoundSoFar    = group + head
        val stillToSearch      = (cellsWithSameColor ++ toSearch.tail) -- cellsFoundSoFar
        find(stillToSearch, cellsFoundSoFar)
      }
    }

    getCellState(board, position) match {
      case Filled(color) =>
        val positions = find(Set(position), Set.empty)
        if (positions.size > 1) {
          Some(Group(color, positions))
        } else {
          None
        }
      case _ => None
    }
  }

  private def removeGroup(board: Board, group: Group): Board = {
    board.map {
      case (column, colIndex) =>
        column.map {
          case (cell, rowIndex) =>
            if (group.positions.contains(Position(colIndex, rowIndex))) {
              Empty
            } else {
              cell
            }
        }.shiftDown
    }.shiftLeft
  }

  private def play(board: Board, position: Position): Option[(Board, Int)] = {
    findGroup(board, position)
      .map(g => (removeGroup(board, g), calcScore(g)))
  }

  private def board(game: Game): Board =
    game match {
      case InProgress(board, _) => board
      case Finished(board, _)   => board
    }

  private def cellColor(cellState: CellState): Map[Color, Int] =
    cellState match {
      case Filled(c) => Map(c -> 1)
      case Empty     => Map.empty
    }

  def colors(game: Game): List[Color] =
    board(game).columns
      .map(_.cells.foldMap(cellColor))
      .combineAll
      .keys
      .toList

  def predominantColor(game: Game): Option[Color] =
    board(game).columns
      .map(_.cells.foldMap(cellColor))
      .combineAll
      .toList match {
      case Nil  => None
      case list => list.maxBy(_._2)._1.some
    }

  def color(game: Game, position: Position): Option[Color] =
    getCellState(board(game), position) match {
      case Filled(c) => c.some
      case Empty     => None
    }

  def evaluateGameState(board: Board, score: Int): Game = {
    def isEmpty(board: Board): Boolean = filledCells(board) == 0

    if (hasValidMoves(board)) {
      InProgress(board, score)
    } else if (isEmpty(board)) {
      Finished(board, score + bonus)
    } else {
      Finished(board, score + penalty(filledCells(board)))
    }
  }

  def applyMove(position: Position, game: Game): Game =
    game match {
      case InProgress(board, score) =>
        play(board, position).map { case (b, s) => evaluateGameState(b, s + score) }.getOrElse(game)
      case Finished(_, _) => game
    }

  def legalMoves(game: Game): List[Position] =
    game match {
      case InProgress(board, _) =>
        board.columns.zipWithIndex
          .flatMap { case (col, colIndex) => col.cells.zipWithIndex.map { case (_, rowIndex) => Position(colIndex, rowIndex) } }
          .flatMap(pos => findGroup(board, pos).toList)
          .distinct
          .flatMap(g => g.positions.headOption.toList)
      case Finished(_, _) => Nil
    }

  def score(game: Game): Int =
    game match {
      case InProgress(_, score) => score
      case Finished(_, score)   => score
    }

  def apply(board: List[List[Int]]): Game =
    SameGame.evaluateGameState(Board(board.map(c => Column(c.map(s => Filled(Color(s)))))), 0)
}
