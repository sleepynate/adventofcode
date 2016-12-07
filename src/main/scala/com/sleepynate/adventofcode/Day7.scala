package com.sleepynate.adventofcode

import com.sleepynate.adventofcode.util.PipeOps._
import scala.io.Source

trait Day7 {
  def getInput:List[String] = (getClass.getResourceAsStream("/day7.txt") |>
    Source.fromInputStream).getLines.toList

  def containsABBA(chars: List[Char]):Boolean = {
    if (chars.length <= 3) false
    else chars match {
      case a1 :: b1 :: b2 :: a2 :: t =>
        if (a1 == a2 &&
            b1 == b2 &&
            a1 != b1 &&
            b2 != a2) true
        else containsABBA(b1 :: b2 :: a2 :: t)
      case _ => throw new UnknownError // should never hit, but kills warning
    }
  }

  def containsABBA(s: String): Boolean = containsABBA(s.toList)

  def containsABA(chars: List[Char]):List[(Char, Char)] = {
    def f(chars: List[Char], acc: List[(Char, Char)]):List[(Char, Char)] = {
      if (chars.length <= 2) acc
      else chars match {
        case a1 :: b :: a2 :: t =>
          if (a1 == a2) f(b :: a2 :: t, (a1, b) :: acc)
          else f(b :: a2 :: t, acc)
        case _ => throw new UnknownError // should never hit, but kills warning
      }
    }
    f(chars, List.empty[(Char, Char)])
  }
  def containsABA(chars: String):List[(Char, Char)] = containsABA(chars.toList)

  def separateByBrackets(s: String) = {
    val regex = """\[(\w+)\]""".r
    val inBrackets = regex.findAllIn(s).map { s => val regex(group) = s; group }
    val notInBrackets = regex.split(s)
    (inBrackets, notInBrackets)
  }

  def supportsTLS(s: String):Boolean = {
    val (inBrackets, notInBrackets) = separateByBrackets(s)
    val foundInBrackets = inBrackets.exists(containsABBA)
    val existsOutsideBrackets = notInBrackets.exists(containsABBA)

    !foundInBrackets && existsOutsideBrackets
  }

  def supportsSSL(s: String): Boolean = {
    val (inBrackets, notInBrackets) = separateByBrackets(s)

    def tupleToBABstring = (t:(Char, Char)) => List(t._2, t._1, t._2).mkString

    val map = notInBrackets.map(s => containsABA(s).map(tupleToBABstring))
    val babStrings = for (string <- notInBrackets ;
                          characterCombos <- containsABA(string))
                          yield tupleToBABstring(characterCombos)
    for (string <- inBrackets) {
      for (bab <- babStrings) {
        if (string.contains(bab)) return true
      }
    }

    return false
  }
}
