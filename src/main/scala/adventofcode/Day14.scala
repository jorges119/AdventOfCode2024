package adventofcode

import common.Support.*
import scala.util.chaining._

@main def Day14 = Day(14) { (input, part) =>
  case class Coords(x: Int, y: Int):
    def *(scale: Int) = Coords(x * scale, y * scale)
    def +(c: Coords) = Coords(x + c.x, y + c.y)

  val i =
    input
      .split("\\n")
      .map(_ match {
        case s"p=$a,$b v=$c,$d" => (Coords(a.toInt, b.toInt), Coords(c.toInt, d.toInt))
      })

  val sizes = Coords(101, 103)
  val splits = Coords((sizes.x / 2.ceil).toInt, (sizes.y / 2.ceil).toInt)
  val seconds = 100

  def advance(robots: Array[(Coords, Coords)], seconds: Int) =
    robots.map(r =>
      (r._1 + (r._2 * seconds))
        .pipe(v => Coords(v.x % (sizes.x), v.y % (sizes.y)))
        .pipe(v => Coords((v.x + (sizes.x)) % (sizes.x), (v.y + (sizes.y)) % (sizes.y)))
    )

  part(1) = advance(i, seconds)
    .filter(p => p.x != splits.x && p.y != splits.y)
    .groupBy(_ match {
      case Coords(x, y) if (x < splits.x && y < splits.y) => 0
      case Coords(x, y) if (x > splits.x && y < splits.y) => 1
      case Coords(x, y) if (x < splits.x && y > splits.y) => 2
      case Coords(x, y) if (x > splits.x && y > splits.y) => 3

    })
    .map(_._2.length)
    .reduce(_ * _)

  val secondsTree = LazyList
    .from(0)
    .takeWhile(n =>
      val pos = advance(i, n)
      !pos.exists(p =>
        (1 to 4)
          .flatMap(a => List(Coords(p.x - a, p.y + a), Coords(p.x + a, p.y + a)))
          .forall(pos.contains(_))
      )
    )
    .toList
    .last + 1

  part(2) = secondsTree

// // Draw it if you want!
// val fin = advance(i, secondsTree)
// (0 to sizes.y - 1).foreach(y =>
//   println(
//     (0 to sizes.x - 1)
//       .map(x => if (fin.contains(Coords(x, y))) 'x' else ' ')
//       .mkString
//   )
// )
}
