package adventofcode

import common.Support.*
import scala.util.chaining._

@main def Day08 = Day(8) { (input, part) =>

  case class Coords(x: Int, y: Int):
    def *(scale: Int) = Coords(x * scale, y * scale)
    def +(c: Coords) = Coords(x + c.x, y + c.y)
    def -(c: Coords) = Coords(x - c.x, y - c.y)

  val i =
    input.split("\\n").zipWithIndex.flatMap(r => r._1.zipWithIndex.map(i => (Coords(i._2, r._2), i._1))).toMap

  val size = i.maxBy(p => p._1.x + p._1.y)._1

  part(1) = i
    .filter(_._2 != '.')
    .groupMap(_._2)(_._1)
    .flatMap(g =>
      g._2.toList
        .combinations(2)
        .flatMap(cs =>
          val vector = cs(0) - cs(1)
          List(cs(0) + vector, cs(1) + (vector * -1)).filter(i.isDefinedAt(_))
        )
    )
    .toList
    .distinct
    .length

  part(2) = i
    .filter(_._2 != '.')
    .groupMap(_._2)(_._1)
    .flatMap(g =>
      g._2.toList
        .combinations(2)
        .flatMap(cs =>
          val vector = cs(0) - cs(1)
          val steps = (0 to Array(((size.x + 1) / vector.x).abs, ((size.y + 1) / vector.y).abs).min)
          val back = steps.map(s => cs(0) + (vector * s))
          val forth = steps.map(s => cs(1) + (vector * (s * -1)))
          (back ++ forth).filter(i.isDefinedAt(_))
        )
    )
    .toList
    .distinct
    .length
}
