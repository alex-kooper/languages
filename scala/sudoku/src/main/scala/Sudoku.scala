import scala.io.Source

object GridTypes {
  type Digit = Int
  type Row = Int
  type Column = Int
  type Cell = (Row, Column)
}

import GridTypes._

case class Grid(gridMap: Map[Cell, Digit]) {
  def set(c: Cell, d: Digit) = Grid(gridMap + (c -> d))
  def apply(r: Row, c: Column): Option[Digit] = gridMap.get((r, c))

  private[this] def renderRow(r: Row): String = {
    def c(i: Column) = this(r, i).map(_.toString).getOrElse(".")

    s"${c(0)}  ${c(1)}  ${c(2)} | ${c(3)}  ${c(4)}  ${c(5)} | ${c(6)}  ${c(7)}  ${c(8)}"
  }

  private[this] val separator1 = "        |         |        "
  private[this] val separator2 = "--------+---------+--------"

  def render: String = {
    List(
      renderRow(0), separator1, renderRow(1), separator1, renderRow(2), separator2,
      renderRow(3), separator1, renderRow(4), separator1, renderRow(5), separator2,
      renderRow(6), separator1, renderRow(7), separator1, renderRow(8)
    ).mkString("\n")
  }
}


object Grid {
  def parse(s: String): Grid = {
    def filterLine(l: String) = l.filter(c => c.isDigit || c == '.')

    val lines = s.split('\n').map(filterLine).filter(_.nonEmpty).toSeq

    val cellsWithDigits = for {
      (row, rowNumber) <- lines.zipWithIndex
      (char, columnNumber) <- row.zipWithIndex
      if char.isDigit
    } yield (rowNumber, columnNumber) -> char.asDigit

    Grid(cellsWithDigits.toMap)
  }
}

object SudokuSolver {
  type CellConstraints = Map[Cell, Set[Digit]]

  private def unknownCells(grid: Grid): Seq[Cell] = {
    for {
      r <- 0 until 9
      c <- 0 until 9
      if grid(r, c).isEmpty
    } yield (r, c)
  }

  private def relatedCells(row: Row, column: Column): Set[Cell] = {
    val rowCells = (0 until 9).map((row, _)).toSet
    val columnCells = (0 until 9).map((_, column)).toSet

    def subgridFirst(x: Int) = x / 3 * 3
    def subgridLast(x: Int) = subgridFirst(x) + 3

    val subgridCells = (
        for {
          r <- subgridFirst(row) until subgridLast(row)
          c <- subgridFirst(column) until subgridLast(column)
        } yield (r, c)
      ).toSet

    rowCells ++ columnCells ++ subgridCells
  }

  def initialCellConstraints(grid: Grid): CellConstraints = {
    def relatedCellValues(row: Row, column: Column) =
      relatedCells(row, column)
      .flatMap {
        case (r, c) => grid(r, c)
      }

    def cellConstraint(row: Row, column: Column) =
      (1 to 9).toSet -- relatedCellValues(row, column)

    unknownCells(grid).map {
      case (r, c) => (r, c) -> cellConstraint(r, c)
    }.toMap
  }
}

object Sudoku extends App {
  val s = Source.fromResource("puzzle1.txt").mkString
  println(Grid.parse(s).render)
}
