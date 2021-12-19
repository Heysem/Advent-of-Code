package solutions
import cats._
import cats.effect.IO
import cats.implicits._
import fs2._

object Day03 extends AOCApp (2021,3) {

  def parseToInt(line: String) =
    line.map{
      case '1' => 1
      case '0' => -1
    }.toList

  def zipMonoid[A](implicit mon: Monoid[A]): Monoid[List[A]] = new Monoid[List[A]] {
    override def empty: List[A] = List.empty
    override def combine(x: List[A], y: List[A]): List[A] =
      x.padZipWith(y)((x, y) => {
        val rx = x.getOrElse(mon.empty)
        val ry = y.getOrElse(mon.empty)
        mon.combine(rx, ry)
      })
  }

  def compute(list: List[Int]): Int = {
    val gamma = list.map(x => if (x > 0) 1 else 0)
    val epsilon = gamma.map(x => if (x == 1) 0 else 1)
    Integer.parseInt(gamma.mkString, 2) * Integer.parseInt(epsilon.mkString, 2)
  }

  def toDecimal(list: List[Int]): Int = {
    val binary = list.map(x => if (x > 0) 1 else 0)
    Integer.parseInt(binary.mkString, 2)
  }

  def filterList(list: List[List[Int]], sign: Int): Int= {
    def loop(l: List[List[Int]], n: Int): Int = {
      if (l.size == 1) return toDecimal(l.head)
      val reduced = l.reduce((x,y)=> x.lazyZip(y).map(_ + _))
      if (reduced(n) >= 0) loop(l.filter(x => x(n) == sign), n+1) else loop(l.filter(x => x(n) == -sign), n+1)
    }
    loop(list, 0)
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(parseToInt)
      .compile
      .foldMonoid(zipMonoid)
      .map(compute)
      .map(_.toString)

  override def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(parseToInt)
      .compile
      .toList
      .map(x => filterList(x,1) * filterList(x,-1))
      .map(_.toString)
}
