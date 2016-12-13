package com.sleepynate.adventofcode.test

import com.sleepynate.adventofcode.Day9
import org.scalatest.{FlatSpec, Matchers}

class Day9Test extends FlatSpec with Matchers {

  "getInput" should "get the input" in {
    new Day9 {
      getInput.length should be (10995)
    }
  }

  "decompress" should "parse examples" in {
    new Day9 {
      decompress("ADVENT", false) should be("ADVENT".length)
      decompress("A(1x5)BC", false) should be("ABBBBBC".length)
      decompress("(3x3)XYZ", false) should be ("XYZXYZXYZ".length)
      decompress("(10x2)LNWIKDMACM", false) should be ("LNWIKDMACMLNWIKDMACM".length)

      decompress("(3x3)XYZ", true) should be ("XYZXYZXYZ".length)
      decompress("X(8x2)(3x3)ABCY", true) should be ("XABCABCABCABCABCABCY".length)
      decompress("(27x12)(20x12)(13x14)(7x10)(1x12)A", true) should be (241920)
      decompress("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", true) should be(445)
    }
  }

  "problem 1 solution" should "parse input?" in {
    new Day9 {
      val decompressed = decompress(getInput, false)
      decompressed should be (0)
    }
  }

  "problem 2 solution" should "parse input?" in {
    new Day9 {
      val decompressed = decompress(getInput, true)
      decompressed should be (0)
    }
  }
}
