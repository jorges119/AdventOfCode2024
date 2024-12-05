package adventofcode

import common.Support.*
import scala.util.chaining._

@main def Day05 = Day(5) { (input, part) =>

  val i = input
    .replaceAll(("\\n\\n"), "<>")
    .replaceAll("\\n", "-")
    .split("<>")
    .pipe(b =>
      (
        b(0).split("-").map(_.split("\\|")).groupMap(_(0).toInt)(_(1).toInt),
        b(1).split("-").map(_.split(",").map(_.toInt).toList).toList
      )
    )

  def findIncorrect(pages: List[List[Int]], rules: Map[Int, Array[Int]]) =
    pages.filter(l =>
      l.foldLeft(l.reverse)((b, _) =>
        if (rules.getOrElse(b.head, Array(): Array[Int]).intersect(b.tail).length >= 1) b else b.tail
      ).length == 0
    )

  part(1) = findIncorrect(i._2, i._1)
    .map(r => r(r.length / 2))
    .sum

  part(2) = (i._2 diff findIncorrect(i._2, i._1))
    .map(l => l.sortWith((a, b) => i._1.getOrElse(a, Array(0)).contains(b)))
    .map(r => r(r.length / 2))
    .sum

}
