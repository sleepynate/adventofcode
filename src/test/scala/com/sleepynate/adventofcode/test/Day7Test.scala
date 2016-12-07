package com.sleepynate.adventofcode.test

import com.sleepynate.adventofcode.Day7
import com.sleepynate.adventofcode.util.PipeOps._
import org.scalatest.{FlatSpec, Matchers}

class Day7Test extends FlatSpec with Matchers {

  "sample input" should "predict TLS hosts" in {
    new Day7 {
      "abba[mnop]qrst" |> supportsTLS should be (true)
      "abcd[bddb]xyyx" |> supportsTLS should be (false)
      "aaaa[qwer]tyui" |> supportsTLS should be (false)
      "ioxxoj[asdfgh]zxcvbn" |> supportsTLS should be (true)
    }
  }

  "sample solution" should "count TLS hosts" in {
    new Day7 {
      val input = List("abba[mnop]qrst", "abcd[bddb]xyyx", "aaaa[qwer]tyui", "ioxxoj[asdfgh]zxcvbn")
      input.count(supportsTLS) should be (2)
    }
  }

  "part 1 solution" should "count TLS hosts" ignore {
    new Day7 {
      getInput.count(supportsTLS) should be (0)
    }
  }

  "part 2 sample" should "find ABAs" in {
    new Day7 {
      "aba[bab]xyz" |> supportsSSL should be (true)
      "xyx[xyx]xyx" |> supportsSSL should be (false)
      "zazbz[bzb]cdb" |> supportsSSL should be (true)
      "aaa[kek]eke" |> supportsSSL should be (true)
    }
  }
   "part 2 solution" should "find SSL hosts" ignore {
     new Day7 {
       getInput.count(supportsSSL) should be(0)
     }
   }
}
