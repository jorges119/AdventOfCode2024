package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day09 = Day(9) { (input, part) =>

  val i =
    input.trim
      .grouped(2)
      .map(a => (a(0).toString.toInt, if (a.length == 2) a(1).toString.toInt else 0))
      .zipWithIndex
      .toList

  @tailrec
  def defrag(s: List[((Int, Int), Int)], sum: BigInt, index: Int): BigInt =
    if (s.length == 0 || (s.length == 1 && s.head._1._1 == 0)) return sum
    else
      val r = s.takeWhile(_._1._2 == 0)
      val next = if (s.length > r.length) s(r.length) else ((0, 0), 0)
      val last = if (s.length > r.length + 1) s.last else ((0, 0), 0)
      val alloc = Array(last._1._1, next._1._2).min
      val u = (r :+ next :+ ((alloc, 0), last._2))
        .filter(v => v._1._1 > 0)
        .foldLeft((index, sum))((b, x) =>
          (x._1._1 + b._1, b._2 + (0 until x._1._1).map(i => BigInt(i + b._1)).sum * x._2)
        )
      val newNext = List(((0, next._1._2 - alloc), next._2)).filter(_._1._2 > 0)
      val newLast = List(((last._1._1 - alloc, last._1._2), last._2)).filter(_._1._1 > 0)
      defrag(newNext ++ s.drop(r.length + 1).dropRight(1) ++ newLast, u._2, u._1)

  @tailrec
  def defragFiles(s: List[((Int, Int), Int)], file: Int): List[((Int, Int), Int)] =
    if (file == 0) return s
    else
      val ordered = s.zipWithIndex
      val last = ordered.find(_._1._2 == file).getOrElse(ordered.head)
      ordered.find(s => s._1._1._2 >= last._1._1._1 && s._2 < last._2) match {
        case None        => defragFiles(s, file - 1)
        case Some(value) =>
          defragFiles(
            ((s.take(value._2) :+ ((value._1._1._1, 0), value._1._2) :+ (
              (
                last._1._1._1,
                value._1._1._2 - last._1._1._1
              ), last._1._2
            )) ++ s.drop(value._2 + 1).take(last._2 - (value._2 + 1)) :+ (last._1._1, 0)) ++ s
              .drop(last._2 + 1),
            file - 1
          )
      }

  part(1) = defrag(i, BigInt(0), 0)

  part(2) = defragFiles(i, i.last._2)
    .foldLeft((0, BigInt(0)))((b, n) =>
      (b._1 + n._1._1 + n._1._2, ((0 until n._1._1).map(i => BigInt(i + b._1)).sum * n._2) + b._2)
    )
    ._2
}
