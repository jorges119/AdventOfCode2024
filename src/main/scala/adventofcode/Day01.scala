package adventofcode

import common.Support.*
import scala.util.chaining._

@main def Day01 = Day(1) { (input, part) =>

  val i = input.split("\\n").map(_.split("   ") match { case Array(x, y) => (x.toInt, y.toInt) }).toList
  part(1) = i.unzip.pipe((a, b) => a.sorted zip b.sorted).map(_ match { case (a, b) => (a - b).abs }).sum
  part(2) = i.unzip.pipe((a, b) =>
    b.groupBy(identity).mapValues(_.size).pipe(t => a.map(n => (t.get(n) getOrElse 0) * n).sum)
  )
}
