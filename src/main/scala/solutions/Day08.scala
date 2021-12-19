package solutions
import cats.effect.IO
import fs2._

object Day08 extends AOCApp (2021,8) {

 case class Entry(input: List[String], output: List[String])


 def parse(str: String): Int = {
   val output = str.split("\\|")(1)
   val tokens = output.trim.split("\\s")
   tokens.count(token => token.length == 2 || token.length == 3 || token.length == 4 || token.length == 7)
 }

 val sevenSegment: Map[String, Set[Int]] = Map(
     "top" -> Set(0,2,3,5,6,7,8,9),
     "topRight" -> Set(0,1,2,3,4,7,8,9),
     "topLeft" -> Set(0,4,5,6,8,9),
     "middle" -> Set(2,3,4,5,6,8,9),
     "bottomRight" -> Set(0,1,3,4,5,6,7,8,9),
     "bottomLeft" -> Set(0,2,6,8),
     "bottom" -> Set(0,2,3,5,6,8,9))


//  def solve(entries: List[Entry]): Int = {
//    entries.map { entry =>
//      entry.output.map{ code =>
//        val count = code.count(c => )
//      }
//    }

//  }


 override def part1(input: fs2.Stream[IO, String]): IO[String] = {
   input
     .through(text.lines)
     .filter(_.nonEmpty)
     .fold(0)((acc, str) => acc + parse(str))
     .map(_.toString)
     .compile
     .string
 }

 override def part2(input: fs2.Stream[IO, String]): IO[String] = input.compile.string
}

