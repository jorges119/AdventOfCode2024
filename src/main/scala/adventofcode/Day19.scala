package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day19 = Day(19) { (input, part) =>

  val (patterns, i) =
    input
      .replaceAll("\\n\\n", "-")
      .replaceAll("\\n", "&")
      .split("-")
      .pipe(a => (a(0).trim.split(",").map(_.trim).toList, a(1).split("&").toList))

  @tailrec
  def build(
    designs: List[(String, Long)],
    patterns: List[String],
    options: Long
  ): (Boolean, Long) =
    val possible =
      designs
        .flatMap(d => patterns.filter(p => d._1.startsWith(p)).map(p => (d._1.drop(p.length), d._2)))
        .groupBy(_._1)
        .map(g => (g._1, g._2.map(_._2).sum))
        .toList
    if (possible.isEmpty)
      (options > 0, options)
    else
      build(
        possible,
        patterns,
        options + possible.filter(_._1.isEmpty).map(_._2).sum
      )

  val result = i.map(d => build(List((d, 1)), patterns, 0))

  part(1) = result.filter(_._1).length
  part(2) = result.map(_._2).sum
}
