package solutions

import fs2._
import cats.effect._
import cats.effect.unsafe.implicits.global

object Main extends App{

  val partialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 65
    case 5 => 999
  }

  val lifted = partialFunction.lift
  print(lifted(4))
}
