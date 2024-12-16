package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day15 = Day(15) { (input, part) =>
  case class Coords(x: Int, y: Int):
    def *(scale: Int) = Coords(x * scale, y * scale)
    def +(c: Coords) = Coords(x + c.x, y + c.y)

  val i =
    input.trim
      .replaceAll("\\n\\n", "&")
      .replaceAll("\\n", "%")
      .split("&")
      .pipe(a =>
        (
          a(0)
            .split("%")
            .zipWithIndex
            .flatMap(r => r._1.zipWithIndex.map(c => (Coords(c._2, r._2), c._1)))
            .toMap,
          a(1).replaceAll("%", "").mkString
        )
      )

  val moves = Map(('>' -> Coords(1, 0)), ('<' -> Coords(-1, 0)), ('v' -> Coords(0, 1)), ('^' -> Coords(0, -1)))

  val startPoint = i._1.find(p => p._2 == '@').getOrElse((Coords(0, 0), 'x'))._1

  part(1) = i._2
    .foldLeft((startPoint, i._1))((b, n) =>
      val offset = moves(n)
      val bumps = LazyList.from(1).takeWhile(x => b._2(b._1 + (offset * x)) == 'O')
      val steps = if (bumps.isEmpty) 0 else bumps.last
      val stop = b._2(b._1 + (offset * (steps + 1)))
      if (stop == '#') b
      else
        val newPos = b._1 + offset
        val correction = if (steps > 0) Map((b._1 + (offset * (steps + 1)) -> 'O')) else Map()
        (newPos, b._2 + (newPos -> '@') + (b._1 -> '.') ++ correction)
    )
    ._2
    .filter(v => v._2 == 'O')
    .map(b => (100 * (b._1.y)) + (b._1.x))
    .sum

  val i2 =
    input.trim
      .replaceAll("\\n\\n", "&")
      .replaceAll("\\n", "%")
      .split("&")
      .pipe(a =>
        (
          a(0)
            .replaceAll("\\.", "..")
            .replaceAll("#", "##")
            .replaceAll("O", "[]")
            .replaceAll("@", "@.")
            .split("%")
            .zipWithIndex
            .flatMap(r => r._1.zipWithIndex.map(c => (Coords(c._2, r._2), c._1)))
            .toMap,
          a(1).replaceAll("%", "").mkString
        )
      )

  val startPoint2 = i2._1.find(p => p._2 == '@').getOrElse((Coords(0, 0), 'x'))._1

  @tailrec
  def getBoxes(
    m: Map[Coords, Char],
    s: List[Coords],
    v: Coords,
    correction: List[(Coords, Char)]
  ): (Char, List[(Coords, Char)]) =
    val newBoxes = s.map(x =>
      m(x + v) match {
        case '['     =>
          (
            'O',
            if (v.x != 0) List(v * 2 + x) else List(x + v, x + v + Coords(1, 0)),
            if (v.x != 0) List((x + v, '.'), (x + v + Coords(1, 0), '['), (x + v + Coords(2, 0), ']'))
            else List((x + v, '.'), (x + v + Coords(1, 0), '.'), (v * 2 + x, '['), (v * 2 + x + Coords(1, 0), ']'))
          )
        case ']'     =>
          (
            'O',
            if (v.x != 0) List(v * 2 + x) else List(x + v, x + v + Coords(-1, 0)),
            if (v.x != 0) List((x + v, '.'), (x + v + Coords(-1, 0), ']'), (x + v + Coords(-2, 0), '['))
            else
              List((x + v, '.'), (x + v + Coords(-1, 0), '.'), (v * 2 + x, ']'), (v * 2 + x + Coords(-1, 0), '['))
          )
        case a: Char => (a, List(), List())
      }
    )
    if (newBoxes.exists(_._1 == '#'))
      return ('#', correction)
    else if (newBoxes.forall(_._2.isEmpty))
      return ('.', correction ++ newBoxes.flatMap(_._3))
    else (getBoxes(m, newBoxes.flatMap(_._2), v, correction ++ newBoxes.flatMap(_._3)))

  part(2) = i2._2
    .foldLeft((startPoint2, i2._1))((b, n) =>
      val offset = moves(n)
      val bumps = getBoxes(b._2, List(b._1), offset, List())
      if (bumps._1 == '#') b
      else
        val newPos = b._1 + offset
        (newPos, b._2 ++ bumps._2.reverse.map(c => (c._1 -> c._2)) + (newPos -> '@') + (b._1 -> '.'))
    )
    ._2
    .filter(v => v._2 == '[')
    .map(b => (100 * (b._1.y)) + (b._1.x))
    .sum
}
