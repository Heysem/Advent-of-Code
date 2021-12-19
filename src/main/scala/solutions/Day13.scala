package solutions

import cats.effect.IO
import fs2._

object Day13 extends AOCApp (2021,13){

  def parseInstructions(input: String): (Set[Point], List[Fold]) = {
    val points = input.split("\n").takeWhile(_ != "").map(Point.parse).toSet
    val folds = input.split("\n").dropWhile(_ != "").tail.map(Fold.parse).toList
    (points, folds)
  }

  case class Point(x: Int, y:Int)
  case object Point{
    def parse(input: String): Point =
      input match {
        case s"$x,$y" => Point(x.toInt,y.toInt)
        case _ => sys.error(s"match error, can't parse $input to Point")
      }
  }

  sealed trait Fold
  case class Vertical(x: Int) extends Fold {
    def fold(along: Int)(value: Int): Int = {
      if (value < along) value
      else along - (value - along)
    }
    def apply(point: Point): Point = Point(fold(along = x)(point.x), point.y)
  }
  case class Horizontal(y: Int) extends Fold {
    def fold(along: Int)(value: Int): Int = {
      if (value < along) value
      else along - (value - along)
    }
    def apply(point: Point): Point = Point(point.x, fold(along = y)(point.y))
  }

  def foldOnce(input: String) = {
    val (points, folds) = parseInstructions(input)
    points.map{pt => folds.head match {
      case Horizontal(y) => Horizontal(y)(pt)
      case Vertical(x) => Vertical(x)(pt)
    }}.size
  }

  def foldMultiple(input: String) = {
    val (points, folds) = parseInstructions(input)
    val folded = folds.foldLeft(points){(acc,fold) => acc.map{pt => fold match {
      case Horizontal(y) => Horizontal(y)(pt)
      case Vertical(x) => Vertical(x)(pt)
    }}}

    val (width, height) = (folded.map(_.x).max + 1, folded.map(_.y).max + 1)
    val grid = Array.fill(height, width)('.')
    for {point <- folded} yield grid(point.y)(point.x) = '#'

    grid.map(_.mkString).mkString("\n")
  }

  case object Fold{
    def parse(input: String): Fold = {
      input match {
        case s"fold along x=$x" => Vertical(x.toInt)
        case s"fold along y=$y" => Horizontal(y.toInt)
        case _ => sys.error(s"match error, can't parse '$input' to Fold")
      }
    }
  }

  override def part1(input: fs2.Stream[IO, String]): IO[String] =
    input
      .map(foldOnce)
      .map(_.toString)
      .compile
      .string

  override def part2(input: fs2.Stream[IO, String]): IO[String] =
    input
      .map(foldMultiple)
      .compile
      .string
}
