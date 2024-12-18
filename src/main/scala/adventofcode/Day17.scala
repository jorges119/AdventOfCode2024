package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }

@main def Day17 = Day(17) { (input, part) =>

  val test = """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""

  val test2 = """Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"""

  val (registers, i) =
    input.trim
      .replaceAll("\\n\\n", "-")
      .replaceAll("\\n", "&")
      .split("-")
      .pipe(t =>
        (
          t(0)
            .split("&")
            .map(_ match {
              case s"Register $r: $v" => (r.head -> v.toLong)
            })
            .toMap + ('P' -> -2L),
          t(1).replace("Program: ", "").split(",").map(_.toInt).toList
        )
      )

  def combo(v: Long, reg: Map[Char, Long]): Long =
    v match {
      case 4 => reg('A')
      case 5 => reg('B')
      case 6 => reg('C')
      case 7 => throw Exception("Not valid")
      case a => a
    }

  def operate(op: Int, v: Int, reg: Map[Char, Long]): Map[Char, Long] =
    reg + (op match {
      case 0 => ('A' -> (reg('A') / scala.math.pow(2, combo(v, reg)).toLong))
      case 1 => ('B' -> (reg('B') ^ v))
      case 2 => ('B' -> (combo(v, reg) % 8))
      case 3 => ('P' -> (if (reg('A') == 0) reg('P') else (v - 2)))
      case 4 => ('B' -> (reg('B') ^ reg('C')))
      case 5 => ('O' -> combo(v, reg)  % 8)
      case 6 => ('B' -> (reg('A') / scala.math.pow(2, combo(v, reg)).toLong))
      case 7 => ('C' -> (reg('A') / scala.math.pow(2, combo(v, reg)).toLong))
    })

  @tailrec
  def process(program: List[Int], reg: Map[Char, Long], output: List[Int], check: Boolean = false): String =
    val next = reg('P').toInt + 2
    if (
      next >= program.length || (check && !output.isEmpty && (program(output.length - 1) != output(
        output.length - 1
      )))
    )
      return output.mkString(",")
    else
      process(
        program,
        operate(program(next), program(next + 1), reg.filter(_._1 != 'O') + ('P' -> next)),
        output ++ reg.filter(_._1 == 'O').map(_._2.toInt)
      )

  def search(start: Long, stop: Long, program: List[Int], reg: Map[Char, Long]): Long =
    if (start >= stop) return -1
    else if (process(program, reg + ('A' -> start), List(), true) == i.mkString(","))
      println(s"Solution: $start")
      return start
    else search(start + 1, stop, program, reg)

  part(1) = process(i, registers, List())

  val chunkSize = 1000000L
  val max: Long = Int.MaxValue * 8L
  val min: Long = Int.MaxValue * 2L
  val threads = (((max - min) / chunkSize) + 1).toInt
  println(s"Starting $threads threads")

  val tasks = (threads until 0 by -1)
    .map(n => Future(search((n * chunkSize) + min, (((n + 1) * chunkSize) + min) min max, i, registers)))

  val f = Future.sequence(tasks)

  part(2) = Await.result(f, 240.minute).filter(_ > 0).reduceOption(_ min _).getOrElse(-2)
}
