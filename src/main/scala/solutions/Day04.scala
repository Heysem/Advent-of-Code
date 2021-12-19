package solutions
import cats.effect.IO
import fs2._

object Day04 extends AOCApp (2021, 4){

  case class Board(lines: List[List[Int]]){
    def mapToNumber(f: Int => Int): Board = Board(lines.map(_.map(f)))
    def columns: List[List[Int]] = lines.transpose
  }

  def computeScore(nums: List[Int], boards: List[Board], winningBoard: Boolean): Int = {
    val numberToTurn = nums.zipWithIndex.toMap
    val turnToNumber = numberToTurn.map(_.swap)

    def turns(board: Board): (Board, Int) = {
      val newboard = board.mapToNumber(numberToTurn)
      val row = newboard.lines.map(_.max).min
      val column = newboard.columns.map(_.max).min
      (board, row min column)
    }

    val sortedBoards: List[(Board, Int)] = boards.map(turns).sortBy(_._2)
    val (candidateBoard, turn) = if (winningBoard) sortedBoards.head else sortedBoards.last
    val score = candidateBoard.lines.map { line =>
      line.filter(numberToTurn(_) > turn).sum
    }.sum
    score * turnToNumber(turn)
  }

  def solve(input: Stream[IO, String], winningBoard: Boolean): IO[String] = {
    input
      .through(text.lines)
      .pull
      .uncons1
      .flatMap{
        case Some((head, rest)) =>
          val nums: List[Int] = head.split(",").toList.map(_.toInt)

          val boards: IO[List[Board]] = rest.split(_.isEmpty).filter(_.nonEmpty)
            .map(_.toList.map(_.trim.split("\\s+").toList.map(_.toInt)))
            .map(Board)
            .compile.toList

          val score: IO[Int] = boards.map(computeScore(nums,_,winningBoard))
          Pull.eval(score).flatMap(Pull.output1)
      }
      .stream
      .map(_.toString)
      .compile
      .string
  }

  override def part1(input: Stream[IO, String]): IO[String] = {
    solve(input, true)
  }


  override def part2(input: fs2.Stream[IO, String]): IO[String] = {
    solve(input, false)
  }
}
