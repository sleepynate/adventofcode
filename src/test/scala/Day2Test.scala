package com.sleepynate.adventofcode.test
import com.sleepynate.adventofcode.Day2
import com.sleepynate.adventofcode.util.PipeOps._
import org.scalatest.{FlatSpec, Matchers}

class Day2Test extends FlatSpec with Matchers {

  "input" should "have length 5 when parsed correctly" in {
    new Day2 {
      getInput should have length(5)
    }
  }

  "sample input" should "be easy enough" in {
    new Day2 {
      val res = List("ULL","RRDDD","LURDL","UUUUD") |> followInstructions(PhoneFive)
      res should be(List(PhoneOne, PhoneNine, PhoneEight, PhoneFive))
    }
  }

  "first solution" should "provide the bathroom code" ignore {
    new Day2 {
      val code = getInput |> followInstructions(PhoneFive)
      code should be (List.empty[Key])
    }
  }

  "part 2 example" should "give 5DB3" in {
    new Day2 {
      val code = List("ULL","RRDDD","LURDL","UUUUD") |> followInstructions(PottyFive)
      code should be (List(PottyFive, PottyD, PottyB, PottyThree))
    }
  }

  "part 2 solution" should "so i can potty" ignore {
    new Day2 {
      val code = getInput |> followInstructions(PottyFive)
      code should be (List.empty[Key])
    }
  }
}
