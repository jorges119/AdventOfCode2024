package adventofcode

import common.Support.*
import scala.util.chaining._

@main def Day10 = Day(10) { (input, part) =>
  case class Coords(x: Int, y: Int):
    def +(c: Coords) = Coords(x + c.x, y + c.y)

  val i =
    input
      .split("\\n")
      .zipWithIndex
      .flatMap(r => r._1.zipWithIndex.map(c => (Coords(c._2, r._2), c._1.toString.toInt)))
      .toMap

  val offsets = List(Coords(0, 1), Coords(0, -1), Coords(1, 0), Coords(-1, 0))

  def searchNext(position: Coords, find: Int, history: List[Coords]): List[List[Coords]] =
    if (history.contains(position) || i.getOrElse(position, -1) != find)
      return List()
    else if (find == 9)
      List(history :+ position)
    else
      offsets.flatMap(o => searchNext(position + o, find + 1, history :+ position))

  part(1) = i.filter(_._2 == 0).map(p => searchNext(p._1, 0, List()).map(_.last).distinct.length).sum
  part(2) = i.filter(_._2 == 0).map(p => searchNext(p._1, 0, List()).length).sum
}
