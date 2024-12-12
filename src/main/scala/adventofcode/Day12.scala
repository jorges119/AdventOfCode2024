package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day12 = Day(12) { (input, part) =>
  case class Coords(x: Int, y: Int):
    def *(scale: Int) = Coords(x * scale, y * scale)
    def +(c: Coords) = Coords(x + c.x, y + c.y)
    def -(c: Coords) = Coords(x - c.x, y - c.y)
    def rotCW() = Coords(y, -x)

  val i =
    input
      .split("\\n")
      .zipWithIndex
      .flatMap(r => r._1.zipWithIndex.map(c => (Coords(c._2, r._2), c._1)))
      .toMap

  val offsets = List(Coords(0, 1), Coords(1, 0), Coords(0, -1), Coords(-1, 0))

  @tailrec
  def fenceIt(plots: List[Coords], plant: Char, history: List[Coords]): List[Coords] =
    if (plots.length == 0) history
    else
      fenceIt(
        plots
          .flatMap(plot => offsets.map(plot + _).filter(p => i.getOrElse(p, '!') == plant && !history.contains(p)))
          .distinct,
        plant,
        history ++ plots
      )

  def getPerimeter(points: List[Coords], plant: Char) =
    points.map(p => offsets.map(o => if (i.getOrElse(p + o, '!') != plant) 1L else 0L).sum).sum

  def getSides(points: List[Coords], plant: Char): Long =
    points
      .flatMap(p =>
        offsets
          .map(o =>
            (LazyList
              .from(0)
              .map(n => o.rotCW() * n)
              .takeWhile(s =>
                i.getOrElse(p + s, '!') == plant && i
                  .getOrElse(p + (o + s), '!') != plant
              )
              .map(p + _)
              ++ LazyList
                .from(1)
                .map(n => o.rotCW() * n)
                .takeWhile(s =>
                  i.getOrElse(p - s, '!') == plant && i
                    .getOrElse((p + o) - s, '!') != plant
                )
                .map(p - _)).map(c => s"${c.x}-${c.y}-${o.x}-${o.y}").sorted
          )
      )
      .filter(_.length > 0)
      .groupBy(identity)
      .size

  part(1) = i
    .foldLeft((0L, List(): List[Coords]))((b, n) =>
      if (b._2.contains(n._1)) b
      else fenceIt(List(n._1), n._2, List()).pipe(r => (b._1 + (r.length * getPerimeter(r, n._2)), b._2 ++ r))
    )
    ._1

  part(2) = i
    .foldLeft((0L, List(): List[Coords]))((b, n) =>
      if (b._2.contains(n._1)) b
      else
        fenceIt(List(n._1), n._2, List()).pipe(r => (b._1 + (r.length * getSides(r, n._2)), b._2 ++ r))
    )
    ._1
}
