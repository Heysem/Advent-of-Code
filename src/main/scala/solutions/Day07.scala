package solutions

import cats.effect.IO
import fs2._

object Day07 extends AOCApp (2021,7) {

  def median(list: List[Int]): Int = {
    val sorted = list.sorted
    sorted(list.size / 2)
  }

  def average(list: List[Int]): Int =
    list.sum / list.size

  def fuelTotal(list: List[Int]): Int = {
    val med = median(list)
    list.foldLeft(0)((acc, v) => acc + Math.abs(med - v))
  }

  def fuelTotalIncreasing(list: List[Int]): Int = {
    val avg = average(list)
    list.foldLeft(0)((acc, v) => acc + (0 to Math.abs(avg - v)).sum)
  }

  def parse(str: String): List[Int] = str.split(",").map(_.toInt).toList

  def solve(input: Stream[IO, String], f: List[Int] => Int): IO[String] = {
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(parse)
      .map(f)
      .map(_.toString)
      .compile
      .string
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, fuelTotal)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, fuelTotalIncreasing)
}
