package solutions
import cats.effect.IO
import cats.implicits._
import fs2._

object Day09 extends AOCApp (2021,9){

  case class Coordinate(x: Int, y: Int)

  case class Board(grid: Map[Coordinate,Int]) {
    def neighbours(coordinate: Coordinate): List[Coordinate] = {
      List(
        Coordinate(-1, 0),
        Coordinate(1, 0),
        Coordinate(0, -1),
        Coordinate(0, 1),
      ).flatMap { case Coordinate(dx, dy) => grid.get(Coordinate(coordinate.x + dx, coordinate.y + dy)).map(_ => Coordinate(coordinate.x + dx, coordinate.y + dy)) }
    }

    def lowPoints: Map[Coordinate, Int] = {
      grid.filter{ case (Coordinate(x,y), lvl) => lvl < neighbours(Coordinate(x,y)).map(x => grid.getOrElse(x,-1)).min}
    }

    def basins: List[Set[(Coordinate, Int)]] = lowPoints.keys.toList.map {
      case Coordinate(x,y) => floodFill(Coordinate(x,y), grid.getOrElse(Coordinate(x,y), -1))
    }

    def floodFill(coordinate: Coordinate, level: Int): Set[(Coordinate, Int)] = {
      if (level == 9)
        Set.empty
      else {
        neighbours(coordinate)
          .filter(x => grid.getOrElse(x, -1) > level)
          .foldMap { next =>
            floodFill(next, grid.getOrElse(next, -1))
          } + (coordinate -> level)
      }
    }
  }

  def toBoard(input: Array[Array[String]]): Board = {
    Board(input.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (n, x) =>
        (Coordinate(x, y), n.toInt)
      }
    }.toMap)
  }

  def computeBasin(board: Board): Int = {
    board.basins.map(_.size).sorted(Ordering.Int.reverse).take(3).product
  }

  def computeLowest(board: Board): Int = {
    board.lowPoints.values.foldLeft(0)((acc, v) => acc + v+1)
  }

  def solve(input: Stream[IO, String], f: Board => Int) = {
    input
      .filter(_.nonEmpty)
      .compile
      .string
      .map(str => str.split("\n").map(line => line.split("")))
      .map(toBoard)
      .map(f)
      .map(_.toString)
  }
  override def part1(input: fs2.Stream[IO, String]): IO[String] =
    solve(input, computeLowest)

  override def part2(input: fs2.Stream[IO, String]): IO[String] =
    solve(input, computeBasin)
}
