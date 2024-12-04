package adventofcode

import common.Support.*
import scala.util.matching.Regex

@main def Day04 = Day(4) { (input, part) =>
  case class Coords(x: Int, y: Int):
    def *(scale: Int) = Coords(x * scale, y * scale)
    def +(c: Coords) = Coords(x + c.x, y + c.y)

  val i = input.trim()
  val t = i.split("\\n").zipWithIndex.flatMap(l => l._1.zipWithIndex.map(r => (Coords(r._2, l._2), r._1))).toMap
  val vectors =
    List(-1, 0, 1, -1, 1)
      .combinations(2)
      .flatMap(_.permutations.map(p => Coords(p.head, p.tail.head)))
      .toList

  def findWord(position: Coords) =
    vectors.map(v =>
      if (
        t.getOrElse(position, '.') == 'X' &&
        t.getOrElse(position + v * 1, '.') == 'M' &&
        t.getOrElse(position + v * 2, '.') == 'A' &&
        t.getOrElse(position + v * 3, '.') == 'S'
      ) 1
      else 0
    )

  def findWordX(position: Coords) =
    if (
      List(Coords(-1, -1), Coords(-1, 1))
        .filter(v =>
          t.getOrElse(position, '.') == 'A' &&
            (
              "MS".contains(t.getOrElse(position + v, '.')) &&
                "MS".contains(t.getOrElse(position + v * -1, '.')) &&
                t.getOrElse(position + v, '.') != t.getOrElse(position + v * -1, '.')
            )
        )
        .length == 2
    ) 1
    else 0

  part(1) = t.keySet.toList.flatMap(findWord(_)).sum
  part(2) = t.keySet.toList.map(findWordX(_)).sum
}
