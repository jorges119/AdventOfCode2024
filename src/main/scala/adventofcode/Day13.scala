package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day13 = Day(13) { (input, part) =>
  case class Coords(x: Double, y: Double):
    def *(scale: Double) = Coords(x * scale, y * scale)
    def +(c: Coords) = Coords(x + c.x, y + c.y)

  case class Machine(a: Coords, b: Coords, p: Coords)

  val i =
    input.trim
      .replaceAll("\\n\\n", "/")
      .replaceAll("\\n", "&")
      .split("/")
      .map(_.trim match {
        case s"Button A: X+$ax, Y+$ay&Button B: X+$bx, Y+$by&Prize: X=$px, Y=$py" =>
          Machine(
            Coords(ax.toDouble, ay.toDouble),
            Coords(bx.toDouble, by.toDouble),
            Coords(px.toDouble, py.toDouble)
          )
      })

  def getA(b: Double, m: Machine) =
    (m.p.x - (b * m.b.x)) / m.a.x

  def getB(m: Machine) =
    (m.p.y - ((m.a.y / m.a.x) * m.p.x)) / (m.b.y - ((m.b.x * m.a.y) / m.a.x))

  println(getB(i.drop(2).head))

  part(1) = i
    .map(m =>
      val b = getB(m)
      val a = getA(b, m)
      if ((b - b.round).abs < 1e-3 && (a - a.round).abs < 1e-3)
        a.round.toInt * 3 + b.round.toInt
      else
        0
    )
    .sum

  part(2) = i
    .map(m =>
      val nM = Machine(m.a, m.b, Coords(m.p.x + 10000000000000.0, m.p.y + 10000000000000.0))
      val b = getB(nM)
      val a = getA(b, nM)
      if ((b - b.round).abs < 1e-3 && (a - a.round).abs < 1e-3)
        BigInt(a.round) * 3 + BigInt(b.round)
      else
        BigInt(0)
    )
    .sum
}
