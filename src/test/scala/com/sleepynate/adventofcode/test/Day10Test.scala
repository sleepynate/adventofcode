package com.sleepynate.adventofcode.test

import com.sleepynate.adventofcode.Day10
import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {
  val exampleInstructions = """value 5 goes to bot 2
                              |bot 2 gives low to bot 1 and high to bot 0
                              |value 3 goes to bot 1
                              |bot 1 gives low to output 1 and high to bot 0
                              |bot 0 gives low to output 2 and high to output 0
                              |value 2 goes to bot 2""".stripMargin

  "part 1 example" should "do the correct thing ¯\\_(ツ)_/¯" in {
    new Day10 {
      val bins = Map(0 -> Seq(5), 1 -> Seq(2), 2 -> Seq(3))
      doTheThing(exampleInstructions.lines.toList).bins should be(bins)
    }
  }

  "part 2 solution" should "give the result" in {
    new Day10 {
      val bins = doTheThing(getInput).bins
      bins(0).head * bins(1).head * bins(2).head should be(0)
    }
  }
}
