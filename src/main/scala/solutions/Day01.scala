package solutions
import cats.effect.IO
import cats.implicits._
import fs2._

object Day01 extends AOCApp(2021, 1) {

  def solve(input: Stream[IO, String], groupSize: Int): IO[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(_.toInt)
      .sliding(groupSize + 1)
      .filter(c => c.last > c.head)
      .compile
      .count
      .map(_.toString)

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, 1)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, 3)

}
