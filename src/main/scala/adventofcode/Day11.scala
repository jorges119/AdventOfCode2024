package adventofcode

import common.Support.*
import scala.util.chaining._

@main def Day11 = Day(11) { (input, part) =>

  val i = input.trim.split(" ")

  def blink(stones: Array[(String, Long)], n: Int): Long =
    if (n == 0) stones.map(_._2).sum
    else
      blink(
        stones
          .flatMap(_ match {
            case ("0", n) => List(("1", n))
            case (a, n) if (a.length() % 2) == 0 =>
              a.splitAt(a.length / 2).pipe(l => List(l._1, l._2).map(v => (v.toLong.toString, n)))
            case (b, n) => List(((b.toLong * 2024).toString, n))
          })
          .groupBy(_._1)
          .map(g => (g._1, g._2.map(_._2).sum))
          .toArray,
        n - 1
      )

  part(1) = blink(i.map(s => (s, 1L)), 25)
  part(2) = blink(i.map(s => (s, 1L)), 75)

}
