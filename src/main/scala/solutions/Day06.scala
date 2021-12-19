package solutions
import cats.effect.IO
import fs2._

object Day06 extends AOCApp(2021,6) {

  def toSchoolofFish(str: String): Array[Long] = {
    str.split(",").map(_.toInt).foldLeft(Array.fill(9)(0L))((array, v) => array.updated(v, array(v) + 1))
  }

  def computeFish(fish: Array[Long], i: Int): Long =
    if (i > 0) computeFish((fish.tail :+ fish.head).updated(6, fish(7) + fish.head), i - 1) else fish.sum

  def solve(input: Stream[IO, String], days: Int): IO[String] =
    input.
      through(text.lines)
      .filter(_.nonEmpty)
      .map(toSchoolofFish)
      .map(x => computeFish(x, days))
      .map(_.toString)
      .compile
      .string

  override def part1(input: fs2.Stream[IO, String]): IO[String] =
    solve(input, 80)

  override def part2(input: fs2.Stream[IO, String]): IO[String] =
    solve(input, 256)
}
