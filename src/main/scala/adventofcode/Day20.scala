package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day20 = Day(20) { (input, part) =>
  case class Coords(x: Int, y: Int):
    def *(scale: Int) = Coords(x * scale, y * scale)
    def +(c: Coords) = Coords(x + c.x, y + c.y)
    def |(c: Coords) = (x - c.x).abs + (y - c.y).abs

  val i =
    input
      .split("\\n")
      .zipWithIndex
      .flatMap(r => r._1.zipWithIndex.map(c => (Coords(c._2, r._2), c._1)))
      .toMap

  val offsets = List(Coords(0, 1), Coords(0, -1), Coords(1, 0), Coords(-1, 0))

  val start = i.find(_._2 == 'S').getOrElse(i.head)._1

  @tailrec
  def race(
    track: List[Coords]
  ): List[Coords] =
    if (i(track.last) == 'E') track
    else
      val newPos = offsets
        .filter(o =>
          val p = track.last + o
          i.getOrElse(p, '#') != '#' && !track.contains(p)
        )
        .head + track.last

      race(
        track :+ newPos
      )

  val trackMap = race(List(start)).reverse.zipWithIndex.map(v => (v._1 -> v._2)).reverse
  val max = trackMap.map(_._2).max

  part(1) = trackMap
    .flatMap(p =>
      offsets
        .filter(o =>
          i.getOrElse(p._1 + o, '-') == '#' && List('E', '.').contains(i.getOrElse(p._1 + (o * 2), '-'))
        )
        .map(o => (max - p._2) + 2 + trackMap.find(_._1 == p._1 + (o * 2)).getOrElse(trackMap.head)._2)
    )
    .filter(v => (max - v) >= 100)
    .length

  part(2) = trackMap
    .flatMap(p =>
      trackMap.filter(o => ((p._1 | o._1) <= 20) && ((max - 100) >= ((max - p._2) + (p._1 | o._1) + o._2)))
    )
    .length
}
