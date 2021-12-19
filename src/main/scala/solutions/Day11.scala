package solutions
import cats.effect.IO
import fs2._

import scala.collection.mutable

object Day11 extends AOCApp (2021,11){

  type Board = Array[Array[Int]]

  sealed trait Action
  case object IncreaseEnergy extends Action
  case object Flash extends Action

  def step(action: Action, grid: Board, flashingPoints: mutable.HashSet[(Int,Int)], row: Int, col: Int): Unit = {
    val length = grid.length // length == width

    action match {
      case IncreaseEnergy => {
        grid(row)(col) += 1
        val energyLvL = grid(row)(col)
        if (!flashingPoints.contains((row, col)) && energyLvL > 9) {
          step(Flash, grid, flashingPoints, row, col)
        }
      }
      case Flash => {
        flashingPoints.add((row, col))
        //top left
        if (row - 1 >= 0 && col - 1 >= 0) {
          step(IncreaseEnergy, grid, flashingPoints, row - 1, col - 1)
        }
        //top
        if (row - 1 >= 0) {
          step(IncreaseEnergy, grid, flashingPoints, row - 1, col)
        }
        //top right
        if (row - 1 >= 0 && col + 1 < length) {
          step(IncreaseEnergy, grid, flashingPoints, row - 1, col + 1)
        }
        //left
        if (col - 1 >= 0) {
          step(IncreaseEnergy, grid, flashingPoints, row, col - 1)
        }
        //right
        if (col + 1 < length) {
          step(IncreaseEnergy, grid, flashingPoints, row, col + 1)
        }
        //bottom left
        if (row + 1 < length && col - 1 >= 0) {
          step(IncreaseEnergy, grid, flashingPoints, row + 1, col - 1)
        }
        //bottom
        if (row + 1 < length) {
          step(IncreaseEnergy, grid, flashingPoints, row + 1, col)
        }
        //bottom right
        if (row + 1 < length && col + 1 < length) {
          step(IncreaseEnergy, grid, flashingPoints, row + 1, col + 1)
        }
      }
    }
    flashingPoints.foreach{ case (row,col) => grid(row)(col) = 0}
  }

  def simulate(board: Board): Int = {
    val flashPoints = mutable.HashSet[(Int, Int)]()
    var flashCount = 0
    val size = board.length

    (1 to size * size).foreach(_ => {
      (0 until size).foreach(row => {
        (0 until size).foreach(col => {
          step(IncreaseEnergy, board, flashPoints, row, col)
        })
      })

      flashCount += flashPoints.size
      flashPoints.clear()
    })
    flashCount
  }

  def synchronize(grid: Board): Int = {
    val flashingSet = mutable.HashSet[(Int, Int)]()
    var count = 0
    val length = grid.length

    while (flashingSet.size != 100) {
      flashingSet.clear()

      (0 until length).foreach(row => {
        (0 until length).foreach(col => {
          step(IncreaseEnergy, grid, flashingSet, row, col)
        })
      })
      count += 1
    }
    count
  }

  def toBoard(input: String): Board =
    input.split("\n").map(line => line.split("").map(_.toInt))

  def solve(input: Stream[IO, String], f: Board => Int): IO[String] =
    input
      .filter(_.nonEmpty)
      .compile
      .string
      .map(toBoard)
      .map(f)
      .map(_.toString)

  override def part1(input: fs2.Stream[IO, String]): IO[String] =
    solve(input, simulate)

  override def part2(input: fs2.Stream[IO, String]): IO[String] =
    solve(input, synchronize)
}
