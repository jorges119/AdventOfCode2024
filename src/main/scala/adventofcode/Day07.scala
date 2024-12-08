package adventofcode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Day07 = Day(7) { (input, part) =>

  val i = input
    .split("\\n")
    .map(
      _.split(": ")
        .pipe(a => (BigInt(a(0)), a(1).split(" ").map(n => BigInt(n)).toList))
    )
    .toList

  val operations = Map(
    'm' -> ((a: BigInt, b: BigInt) => a * b),
    's' -> ((a: BigInt, b: BigInt) => a + b)
  )

  def nextStep(
    ops: Map[Char, (BigInt, BigInt) => BigInt],
    list: List[BigInt],
    buffer: BigInt,
    target: BigInt
  ): Boolean =
    if (target == buffer && list.length == 0)
      true
    else if (list.length == 0 || buffer > target)
      false
    else
      ops.exists(o => nextStep(ops, list.tail, o._2(buffer, list.head), target))

  part(1) = i.filter(r => nextStep(operations, r._2.tail, r._2.head, r._1)).map(_._1).sum

  part(2) = i
    .filter(r =>
      nextStep(
        operations + ('c' -> ((a: BigInt, b: BigInt) => BigInt(a.toString + b.toString))),
        r._2.tail,
        r._2.head,
        r._1
      )
    )
    .map(_._1)
    .sum

}
