package com.sleepynate.adventofcode.test

import com.sleepynate.adventofcode.util.PipeOps._
import com.sleepynate.adventofcode.Day6
import org.scalatest.{FlatSpec, Matchers}

class Day6Test extends FlatSpec with Matchers {

  val sampleInput = """eedadn
                      |drvtee
                      |eandsr
                      |raavrd
                      |atevrs
                      |tsrnev
                      |sdttsa
                      |rasrtv
                      |nssdts
                      |ntnada
                      |svetve
                      |tesnvt
                      |vntsnd
                      |vrdear
                      |dvrsen
                      |enarar""".stripMargin

  "sample input" should "say easter" in {
    new Day6 {
      val message = sampleInput.lines.toList |> transpose |> mostCommonInRow
      message.mkString should be ("easter")
    }
  }
  "part 1 solution" should "not say easter" ignore {
    new Day6 {
      val message = getInput |> transpose |> mostCommonInRow
      message.mkString should be ("easter")
    }
  }

  "part 2 sample input" should "say advent" in {
    new Day6 {
      val message = sampleInput.lines.toList |> transpose |> leastCommonInRow
      message.mkString should be("advent")
    }
  }
  "part 2 solution" should "not say easter" ignore {
    new Day6 {
      val message = getInput |> transpose |> leastCommonInRow
      message.mkString should be ("easter")
    }
  }
}
