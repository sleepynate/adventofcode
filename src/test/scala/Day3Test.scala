import com.sleepynate.adventofcode.Day3
import com.sleepynate.adventofcode.util.PipeOps._
import org.scalatest.{FlatSpec, Matchers}

class Day3Test extends FlatSpec with Matchers {
  "input" should "be readable" in {
    new Day3 {
      getInput should have length 1599
    }
  }

  "input" should "be split into triples" in {
    new Day3 {
      val triples = getInput |> splitInput
      triples should have size 1599
      triples.head should have size 3
    }
  }

  "checkSides" should "check sides of a triangle" in {
    new Day3 {
      checkSides(List(3,4,5)) should be(true)
      checkSides(List(5,10,25)) should be(false)
    }
  }

  "part 1" should "have a solution" ignore {
    new Day3 {
      val res = (getInput |> splitInput).filter(p => checkSides(p.toList)).length
      res should be (0)
    }
  }

  "transposed triangles" should "get pulled out in threes" in {
    new Day3 {
      val res = List("1 2 3", "4 5 6", "7 8 9") |> splitInput |> transposeInput
      res.head should be(List(1, 4, 7))
    }
  }

  "part 2" should "be easy now" ignore {
    new Day3 {
      val res = (getInput |> splitInput |> transposeInput).filter(p => checkSides(p.toList))
      res should have length (0)
    }
  }
}
