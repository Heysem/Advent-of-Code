package solutions
import cats.effect.IO
import fs2._
import cats.implicits._

object Day05 extends AOCApp (2021,5){

  case class Point(x: Int, y: Int)

  case class Line(from: Point, to: Point){
    def isNotDiagonal: Boolean = {
       from.x == to.x || from.y == to.y
    }
    def coordinates: List[Point] = {
      if (this.isNotDiagonal) {
        val xs = if (from.x < to.x) (from.x to to.x).toList else (to.x to from.x).toList
        val ys = if (from.y < to.y) (from.y to to.y).toList else (to.y to from.y).toList
        for {
          x <- xs
          y <- ys
        } yield Point(x, y)
      } else {
        val xs = if (from.x < to.x) (from.x to to.x).toList else (from.x to to.x by -1).toList
        val ys  = if (from.y < to.y) (from.y to to.y).toList else (from.y to to.y by -1).toList
        (xs zip ys).map{case(x,y) => Point(x, y)}
      }
    }
  }

  def toLine(str: String): Line = {
    val pts = str.split("\\s->\\s")
      .map(x => x.split(','))
      .map(x => Point(x(0).toInt,x(1).toInt))
    Line(pts(0), pts(1))
  }

  override def part1(input: fs2.Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(toLine)
      .filter(_.isNotDiagonal)
      .compile
      .fold(Map[Point, Int]()) {
        (acc, line) =>
          line.coordinates.foldLeft(acc) { (m, p) => m.updatedWith(p) {
            case Some(n) => Some(n + 1)
            case None => Some(1)
          }}
      }
      .map(_.count(_._2 >= 2))
      .map(_.toString)


  override def part2(input: fs2.Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(toLine)
      .compile
      .fold(Map[Point, Int]()) {
        (acc, line) =>
          line.coordinates.foldLeft(acc) { (m, p) => m.updatedWith(p) {
            case Some(n) => Some(n + 1)
            case None => Some(1)
          }}
      }
      .map(_.count(_._2 >= 2))
      .map(_.toString)
}
