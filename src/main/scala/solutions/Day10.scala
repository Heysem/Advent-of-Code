package solutions
import cats.effect.IO
import fs2._

import scala.collection.mutable
import scala.collection.mutable.Stack

object Day10 extends AOCApp (2021, 10) {

  val points: Map[Char, Long] = Map(
    '}' -> 1197,
    ')' -> 3,
    '>' -> 25137,
    ']' -> 57)

  val points2: Map[Char, Long] = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4)

  val mirror: Map[Char, Char] = Map(
    '}' -> '{',
    ')' -> '(',
    '>' -> '<',
    ']' -> '[')

  val reverse: Map[Char, Char] = Map(
    '{' -> '}',
    '(' -> ')',
    '<' -> '>',
    '[' -> ']')

  def parseLine(str: String): Long = {
    val chars = str.toCharArray
    val stack: Stack[Char] = new Stack()
    chars.foreach {
      case ch@'{' => stack.push(ch)
      case ch@'<' => stack.push(ch)
      case ch@'[' => stack.push(ch)
      case ch@'(' => stack.push(ch)
      case x if stack.isEmpty || mirror(x) != stack.pop() => return points(x)
      case _ =>
    }
    0
  }

  def completeString(str: String): Long = {
    val chars = str.toCharArray
    val stack: Stack[Char] = mutable.Stack()
    chars.foreach {
      case ch@'{' => stack.push(ch)
      case ch@'<' => stack.push(ch)
      case ch@'[' => stack.push(ch)
      case ch@'(' => stack.push(ch)
      case _ => stack.pop
    }
    val list = stack.map(char => reverse(char)).toList
    list.foldLeft(0L)((acc, v) => (5L * acc) + points2(v))
  }

  def midPoint(list: List[Long]): Long = {
    val sortedList = list.sorted
    sortedList(list.size / 2)
  }

  override def part1(input: fs2.Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .fold(0L)((acc, v) => acc + parseLine(v))
      .map(_.toString)
      .compile
      .string


  override def part2(input: fs2.Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .filter(x => parseLine(x) == 0)
      .map(completeString)
      .compile
      .toList
      .map(midPoint)
      .map(_.toString)
}
