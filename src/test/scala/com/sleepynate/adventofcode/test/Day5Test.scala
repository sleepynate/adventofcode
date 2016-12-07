package com.sleepynate.adventofcode.test

import com.sleepynate.adventofcode.Day5
import org.scalatest.{FlatSpec, Matchers}

class Day5Test extends FlatSpec with Matchers {

  "md5s checksums" should "be generated" in {
    new Day5 {
      md5("abc") should be("900150983cd24fb0d6963f7d28e17f72")
    }
  }

  "haystack generator" should "concat seeds to integers" in {
    new Day5 {
      val hay = haystack("abc").take(10)
      hay.head should be ("abc1")
      hay.last should be ("abc10")
    }
  }

  "part 1 test case" should "provide given password" ignore {
    new Day5 {
      val password = zeroHashes("abc",_.startsWith("00000")).take(8)
      password.toList.map(_.charAt(5)).mkString should be("18f47a30")
    }
  }

  "part 1 actual" should "provide correct password" ignore {
    new Day5 {
      val password = zeroHashes("ffykfhsq", _.startsWith("00000")).take(8)
      password.toList.map(_.charAt(5)).mkString should be("18f47a30")
    }
  }

  "part 2 test case" should "provide given password" ignore {
    new Day5 {
      def password = zeroHashes("abc", s => s.startsWith("00000") && s.charAt(5) >= '0' && s.charAt(5) < '8')
      val passString = foldUntil8(password).sortBy(_._1).map(_._2).mkString

      passString should be("05ace8e3")
    }
  }

  "part 2 actual" should "provide given password" ignore {
    new Day5 {
      def password =  zeroHashes("ffykfhsq", s => s.startsWith("00000") && s.charAt(5) >='0' && s.charAt(5) < '8')
      val passString = foldUntil8(password).sortBy(_._1).map(_._2).mkString
      println(passString)

      passString should be("05ace8e3")
    }
  }
}
