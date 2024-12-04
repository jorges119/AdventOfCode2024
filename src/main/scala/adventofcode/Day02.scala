package adventofcode

import common.Support.*
import scala.util.chaining._

@main def Day02 = Day(2) { (input, part) =>

  def safeSearch(l: Seq[Int]): Boolean = l
    .sliding(2)
    .map(a => a(0) - a(1))
    .toList
    .pipe(b => b.exists(v => !(1 to 3).contains(v.abs)) || b.map(_.sign).distinct.length > 1)

  val i = input.split("\\n").map(_.split(" ").map(_.toInt)).toList
  part(1) = i.filter(!safeSearch(_)).length
  part(2) = i.filter(r => !(1 to r.length).map(n => safeSearch(r.take(n - 1) ++ r.drop(n))).reduce(_ && _)).length
}
