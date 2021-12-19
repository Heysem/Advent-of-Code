package solutions

import cats.effect._
import cats.effect.std._
import fs2._
import fs2.io.file.{Files, Path}
import org.http4s._
import org.http4s.implicits._
import org.http4s.jdkhttpclient.JdkHttpClient

abstract class AOCApp(year: Int, day: Int) extends IOApp {

  def part1(input: Stream[IO, String]): IO[String]
  def part2(input: Stream[IO, String]): IO[String]

  override def run(args: List[String]): IO[ExitCode] = {
    val aocToken = IO("_ga=GA1.2.1164839474.1633526932; session=53616c7465645f5fed8025720e264dbfc4f626e54971de0f3945f236e744793aac67374ed206c7755a8ccd6bc8646283; _gid=GA1.2.1874603159.1638304012")

    JdkHttpClient.simple[IO].use { client =>
      aocToken.flatMap { sessionCookie =>
        val req = Request[IO](
          uri = uri"https://adventofcode.com" / year.toString / "day" / day.toString / "input"
        ).addCookie("session", sessionCookie)
        val body = client.stream(req).flatMap(_.body).through(text.utf8.decode)
        val testBody = Files[IO].readAll(Path("src/main/scala/solutions/testInput.txt")).through(text.utf8.decode)

        part1(body).flatMap(Console[IO].println(_)) >>
          part2(body).flatMap(Console[IO].println(_))
      }
    }.as(ExitCode.Success)
  }

}
