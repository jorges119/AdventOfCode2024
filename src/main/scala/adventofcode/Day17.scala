package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day17 = Day(17) { (input, part) =>

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
  def process(program: List[Int], reg: Map[Char, Long], output: List[Int], check: Boolean = false): List[Int] =
    val next = reg('P').toInt + 2
    if (
      next >= program.length || (check && !output.isEmpty && (program(output.length - 1) != output(
        output.length - 1
      )))
    )
      return output
    else
      process(
        program,
        operate(program(next), program(next + 1), reg.filter(_._1 != 'O') + ('P' -> next)),
        output ++ reg.filter(_._1 == 'O').map(_._2.toInt)
      )

  part(1) = process(i, registers, List()).map(_.toString).mkString(",")

  part(2) = (0 to i.length)
    .foldLeft(List(0x000000000000L))((b, n) =>
      b.flatMap(l =>
        (0 to 7)
          .map(v =>
            val newV = (l + (v.toLong << ((i.length - n) * 3)))
            (newV, process(i, registers + ('A' -> newV), List()))
          )
      ).filter(_._2.reverse.take(n) == i.reverse.take(n))
        .map(_._1)
    )
    .min
}
