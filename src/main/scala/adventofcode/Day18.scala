package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day18 = Day(18) { (input, part) =>
  case class Coords(x: Int, y: Int):
    def +(c: Coords) = Coords(x + c.x, y + c.y)
    def rotCW() = Coords(y, -x)
    def rotCCW() = Coords(-y, x)

  val i =
    input
      .split("\\n")
      .map(_.trim.split(",").pipe(x => Coords(x(0).toInt, x(1).toInt)))
      .toList

  val size = 70
  val numBytes = 1024

  @tailrec
  def race(
    bytes: List[Coords],
    start: List[(Coords, Coords, (List[Coords], Long))],
    visited: Map[(Coords, Coords), Long]
  ): Long =
    if (start.isEmpty) -1
    else if (start.exists(p => p._1.y == size && p._1.x == size))
      start.filter(_._1.y == size).map(_._3._1.length).min - 1
    else
      val next = start
        .flatMap(s =>
          List(
            (s._1, s._2, s._3._2 + 1, s._3._1),
            (s._1, s._2.rotCCW(), s._3._2 + 1, s._3._1),
            (s._1, s._2.rotCW(), s._3._2 + 1, s._3._1)
          )
            .filter(p =>
              !bytes.contains(p._1 + p._2) && (p._1 + p._2).x >= 0 && (p._1 + p._2).x <= size && (
                p._1 + p._2
              ).y >= 0 && (p._1 + p._2).y <= size
            )
        )
        .groupBy(p => (p._1 + p._2, p._2))
        .map(_._2.minBy(_._3))
        .filter(p => visited.getOrElse((p._1 + p._2, p._2), Long.MaxValue) > p._3)
        .toList

      race(
        bytes,
        next.map(p => (p._1 + p._2, p._2, (p._4 :+ (p._1 + p._2), p._3))),
        visited ++ next.map(p => ((p._1 + p._2, p._2) -> p._3)).toMap
      )

  // Reuse day 16 solution
  part(1) = race(
    i.take(numBytes),
    List(
      (Coords(0, 0), Coords(1, 0), (List(Coords(0, 0)), 0L)),
      (Coords(0, 0), Coords(0, -1), (List(Coords(0, 0)), 0L))
    ),
    Map((Coords(0, 0), Coords(1, 0)) -> 0, (Coords(0, 0), Coords(0, -1)) -> 0)
  )

  // Lazy slow
  part(2) = i(
    (1 until i.length - numBytes)
      .takeWhile(n =>
        race(
          i.take(numBytes + n),
          List(
            (Coords(0, 0), Coords(1, 0), (List(Coords(0, 0)), 0L)),
            (Coords(0, 0), Coords(0, -1), (List(Coords(0, 0)), 0L))
          ),
          Map((Coords(0, 0), Coords(1, 0)) -> 0, (Coords(0, 0), Coords(0, -1)) -> 0)
        ) != -1
      )
      .last + numBytes
  )
}
