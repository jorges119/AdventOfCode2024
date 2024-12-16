package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day16 = Day(16) { (input, part) =>
  case class Coords(x: Int, y: Int):
    def +(c: Coords) = Coords(x + c.x, y + c.y)
    def rotCW() = Coords(y, -x)
    def rotCCW() = Coords(-y, x)

  val i =
    input
      .split("\\n")
      .zipWithIndex
      .flatMap(r => r._1.zipWithIndex.map(c => (Coords(c._2, r._2), c._1)))
      .toMap

  val startPoint = i.find(_._2 == 'S').getOrElse((Coords(0, 0), 'x'))._1

  @tailrec
  def race(
    mode: "SLOW" | "FAST",
    start: List[(Coords, Coords, (List[Coords], Long))],
    visited: Map[(Coords, Coords), Long],
    done: List[(List[Coords], Long)] = List()
  ): (Long, Int) =
    if (start.isEmpty)
      val score = done.minBy(_._2)._2
      (score, done.filter(_._2 == score).flatMap(_._1).distinct.length)
    else
      val finished = start.filter(l => i.getOrElse(l._3._1.last, '#') == 'E')
      val next = start
        .flatMap(s =>
          List(
            (s._1, s._2, s._3._2 + 1, s._3._1),
            (s._1, s._2.rotCCW(), s._3._2 + 1001, s._3._1),
            (s._1, s._2.rotCW(), s._3._2 + 1001, s._3._1)
          )
            .filter(p => i.getOrElse((p._1 + p._2), '#') != '#')
        )
        .pipe(r =>
          (if (mode == "FAST")
             r.groupBy(p => (p._1 + p._2, p._2)).map(_._2.minBy(_._3))
           else r)
        )
        .filter(p => visited.getOrElse((p._1 + p._2, p._2), Long.MaxValue) > p._3)
        .toList

      race(
        mode,
        next.map(p => (p._1 + p._2, p._2, (p._4 :+ (p._1 + p._2), p._3))),
        visited ++ next.map(p => ((p._1 + p._2, p._2) -> p._3)).toMap,
        done ++ finished.map(_._3)
      )

  val start = System.currentTimeMillis;

  part(1) =
    race("FAST", List((startPoint, Coords(1, 0), (List(startPoint), 0L))), Map((startPoint, Coords(1, 0)) -> 0))._1

  val next = System.currentTimeMillis;

  part(2) =
    race("SLOW", List((startPoint, Coords(1, 0), (List(startPoint), 0L))), Map((startPoint, Coords(1, 0)) -> 0))._2

  val end = System.currentTimeMillis

  println(s"FAST method solved in ${next - start}ms, SLOW method solved in ${end - next}ms")

}
