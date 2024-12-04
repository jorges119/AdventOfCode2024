package infiAdventOfCode

import common.Support.*
import scala.util.chaining._
import scala.annotation.tailrec

@main def Infi01() =
  val input = getInfiInput(sys.env("INFI_CODE"))
  println("Collected data")

  @tailrec
  def operate(coords: (Int, Int, Int), stack: List[Int], lines: List[String]): Int =
    val operation = lines.head.split(" ")
    val op = operation(0)
    val value =
      if (operation.length > 1)
        operation(1) match {
          case "X" => coords._1
          case "Y" => coords._2
          case "Z" => coords._3
          case x   => x.toInt
        }
      else
        0
    val newStack = op match {
      case "push"  => value +: stack
      case "add"   => (stack.head + stack.tail.head) +: stack.drop(2)
      case "jmpos" => stack.tail
      case "ret"   => stack.take(1)
    }
    if (op == "ret")
      newStack.head
    else
      operate(coords, newStack, lines.drop(if (op == "jmpos" && stack.head >= 0) (value + 1) else 1))

  val i = input.split("\\n").map(_.trim()).toList
  val res1 = (0 until 30)
    .flatMap(x => (0 until 30).flatMap(y => (0 until 30).map(z => ((x, y, z), operate((x, y, z), List(), i)))))
    .toMap

  // Part 1
  println(s"Part 1: ${res1.values.sum}")

  val vectors = List((-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1))

  def followCloud(position: (Int, Int, Int), visited: List[(Int, Int, Int)]): (List[(Int, Int, Int)], Int) =
    if (List(position._1, position._2, position._3).exists(p => p < 0 || p > 29)) return (visited, 0)
    else if (visited.contains(position) || res1.getOrElse(position, 0) < 1) return (visited :+ position, 0)
    else
      (
        vectors
          .map(offset => (position._1 + offset._1, position._2 + offset._2, position._3 + offset._3))
          .filter(!visited.contains(_))
          .foldLeft((visited :+ position, 1))((v, position) => followCloud(position, v._1))
          ._1,
        1
      )

  val res2 = res1.keySet
    .foldLeft((List(): List[(Int, Int, Int)], 0)) { (b, coords) =>
      var res = followCloud(coords, b._1)
      (res._1, res._2 + b._2)
    }
    ._2

  // Part 2
  println(s"Part 2: $res2")
