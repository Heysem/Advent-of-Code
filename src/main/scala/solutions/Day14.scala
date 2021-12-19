package solutions
import cats.effect.IO
import fs2._

object Day14 extends AOCApp (2021,14){

  def solve(input: String, steps: Int) = {
    val (template, pairs) = parse(input)
    val templateMap = templateToMap(template)
    val expanded = expandPolymers(templateMap, steps, pairs)
    score(expanded)
  }

  def score(input: Map[String, Long]): Long = {
    val occurrences = input.foldLeft(Map.empty[Char, Long]){(acc, v) =>
      val arr = v._1.toCharArray
      acc.updatedWith(arr(0)){
        case Some(x) => Some(x + v._2)
        case None => Some(v._2)
      }.updatedWith(arr(1)){
        case Some(x) => Some(x + v._2)
        case None => Some(v._2)
      }
    }
    (occurrences.values.max - occurrences.values.min) / 2
  }

  def templateToMap(input: String): Map[String, Long] = {
    input.sliding(2).foldLeft(Map.empty[String,Long]){(acc, v) =>
      acc.updatedWith(v){
        case Some(x) => Some(x+1)
        case None => Some(1)
      }
    }
  }

  def parse(input: String) = {
    val template = input.split("\n").take(1).mkString
    val pairs = input.split("\n").dropWhile(_ != "").tail
    val pairMap = pairs.foldLeft(Map.empty[String,String]){(acc, kv) => kv match {
      case s"$x -> $y" => acc.updated(x,y)
    }}
    (template, pairMap)
  }

  def expandPolymers(state: Map[String, Long], n: Int, pairs: Map[String,String]): Map[String, Long] = {
    if(n == 0) state else {
      val newstate = state.foldLeft(Map.empty[String, Long]) { (acc, kv) =>
        val result = pairs(kv._1)
        val (leftPair, rightPair, value) = (kv._1.substring(0, 1) + result, result + kv._1.substring(1, 2), kv._2)
        acc.updatedWith(leftPair){
          case Some(v) => Some(v + value)
          case None => Some(value)
        }.updatedWith(rightPair) {
          case Some(v) => Some(v  + value)
          case None => Some(value)
        }
      }
      expandPolymers(newstate, n -1, pairs)
    }
  }

  override def part1(input: fs2.Stream[IO, String]): IO[String] =
    input
      .map(solve(_,10))
      .map(_.toString)
      .compile
      .string

  override def part2(input: fs2.Stream[IO, String]): IO[String] =
    input
      .map(solve(_,40))
      .map(_.toString)
      .compile
      .string
}
