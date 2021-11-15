// Facebook Interview Sample

// 2D Spiral Array

// Find the pattern and complete the function:
// int[][] spiral(int n);
// where n is the size of the 2D array.

// Sample Result

// input = 3
// 123
// 894
// 765

// input = 4
// 01 02 03 04
// 12 13 14 05
// 11 16 15 06
// 10 09 08 07

import scala.annotation.tailrec

class Field private (val dimensions: Int, map: Map[(Int, Int), Int] = Map()):
  def set(x: Int, y: Int, n: Int) = Field(dimensions, map + ((x, y) -> n))

  def apply(x: Int, y: Int) = map.get((x, y))

  def isValid(x: Int, y: Int) =
    x >= 0 && x < dimensions && y >= 0 && y < dimensions

object Field:
  def empty(n: Int) = Field(n)

def rotateRight(dx: Int, dy: Int) = (-dy, dx)

def canSet(field: Field, x: Int, y: Int) =
  field.isValid(x, y) && field(x, y).isEmpty

def mk2dSpiral(n: Int) =

  @tailrec
  def go(
      field: Field,
      x: Int,
      y: Int,
      dx: Int,
      dy: Int,
      currentN: Int
  ): Field =
    val newField = field.set(x, y, currentN)

    if canSet(newField, x + dx, y + dy) then
      go(
        field = newField,
        x = x + dx,
        y = y + dy,
        dx = dx,
        dy = dy,
        currentN = currentN + 1
      )
    else
      val (newDx, newDy) = rotateRight(dx, dy)
      if canSet(newField, x + newDx, y + newDy) then
        go(
          field = newField,
          x = x + newDx,
          y = y + newDy,
          dx = newDx,
          dy = newDy,
          currentN = currentN + 1
        )
      else newField

  go(Field.empty(n), x = 0, y = 0, dx = 1, dy = 0, currentN = 1)

def fieldToString(field: Field) =
  val nDigits = (field.dimensions * field.dimensions).toString.length
  val fmt = s"%0${nDigits}d"

  (0 until field.dimensions)
    .map { y =>
      (0 until field.dimensions)
        .map { x =>
          fmt.format(field(x, y).getOrElse(0))
        }
        .mkString(" ")
    }
    .mkString("\n")

@main def print2dSpiral = println(
  fieldToString(
    mk2dSpiral(7)
  )
)
