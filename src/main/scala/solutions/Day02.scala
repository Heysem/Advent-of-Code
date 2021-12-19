package solutions
import cats.effect.{Async, IO}
import fs2._

object Day02 extends AOCApp (2021,2){

  case class Position(horizontal: Int, vertical: Int, aim: Int)
  case class Instruction(direction: String, distance: Int)
  case object Instruction{
    def fromString(line: String): Instruction = {
      val arr = line.split(' ').toList
      Instruction(arr.head, arr(1).toInt)
    }
  }

  def computeDistance(instruction: Instruction): (Int, Int) = {
    instruction.direction match {
      case "forward" => (instruction.distance, 0)
      case "down" => (0,instruction.distance)
      case "up" => (0, -instruction.distance)
    }
  }


  override def part1(input: Stream[IO, String]): IO[String] = {
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(x => Instruction.fromString(x))
      .map(computeDistance)
      .foldMonoid
      .map(x => (x._1 * x._2).toString)
      .compile
      .string
  }

  override def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(x => Instruction.fromString(x))
      .fold(Position(0,0,0)) { (acc, y) =>
        y.direction match {
          case "forward" => Position(acc.horizontal + y.distance, acc.vertical + acc.aim * y.distance, acc.aim)
          case "down" => Position(acc.horizontal, acc.vertical, acc.aim + y.distance)
          case "up" => Position(acc.horizontal, acc.vertical, acc.aim - y.distance)
        }
      }
      .map(x => (x.horizontal * x.vertical).toString)
      .compile
      .string
}
