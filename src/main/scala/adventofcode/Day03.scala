package adventofcode

import common.Support.*
import scala.util.matching.Regex

@main def Day03 = Day(3) { (input, part) =>

  val i = input.trim()
  val r = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r
  part(1) = r.findAllMatchIn(i).map(m => m.group(1).toInt * m.group(2).toInt).sum
  val r2 = "(mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\))".r
  part(2) = r2
    .findAllMatchIn(i)
    .foldLeft((0, true))((b, m) =>
      m.matched match {
        case "do()"    => (b._1, true)
        case "don't()" => (b._1, false)
        case _         => if (b._2) (m.group(2).toInt * m.group(3).toInt + b._1, b._2) else (b._1, b._2)
      }
    )
    ._1
}
