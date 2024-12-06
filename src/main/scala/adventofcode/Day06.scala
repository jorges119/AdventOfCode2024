package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day06 = Day(6) { (input, part) =>
  def offset(pos: (Int, Int), offset: (Int, Int)) =
    (pos._1 + offset._1, pos._2 + offset._2)

  val i = input.split("\\n").zipWithIndex.flatMap((l, y) => l.zipWithIndex.map((p, x) => ((x, y), p))).toMap
  val dirs = Array('<' -> (-1, 0), '^' -> (0, -1), '>' -> (1, 0), 'v' -> (0, 1))

  @tailrec
  def patrol(
    startPoint: ((Int, Int), Char),
    map: Map[(Int, Int), Char],
    visited: List[((Int, Int), Char)]
  ): (List[((Int, Int), Char)], Int) =
    val position = startPoint._1
    val directions = dirs.map(_._1).indexOf(startPoint._2).pipe(n => dirs.drop(n) ++ dirs.take(n))
    if (visited.contains(startPoint)) (visited, -1)
    else if (!map.isDefinedAt(position)) (visited, map.values.toList.filter(_ == 'X').length)
    else
      val dirChar =
        directions.find(d => map.getOrElse(offset(position, d._2), '.') != '#').getOrElse(directions.head)
      val newOnes = directions.take(directions.indexOf(dirChar) + 1).map(d => (position, d._1))
      patrol(
        (offset(position, dirChar._2) -> dirChar._1),
        map + ((position)             -> 'X'),
        visited ++ newOnes
      )

  val startPoint = i.find((_, c) => dirs.map(_._1).contains(c)).getOrElse((-1, -1), '.')
  val run = patrol(startPoint, i, List())
  part(1) = run._2

  val orderOfVisit = run._1.map(_._1)
  part(2) = run._1.zipWithIndex
    .filter { p =>
      print(s"\t${p._2}/${orderOfVisit.length}\r")
      val obstacle = offset(p._1._1, dirs.toMap.getOrElse(p._1._2, (0, 0)))
      val visited = orderOfVisit.indexOf(obstacle)

      i.getOrElse(obstacle, '#') == '.' &&
      (visited == -1 || visited > p._2) &&
      patrol(p._1, i + (obstacle -> '#'), List())._2 == -1
    }
    .map(p => offset(p._1._1, dirs.toMap.getOrElse(p._1._2, (0, 0))))
    .distinct
    .length
}
