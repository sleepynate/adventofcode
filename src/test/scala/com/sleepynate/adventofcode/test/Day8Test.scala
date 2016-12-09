package com.sleepynate.adventofcode.test

import com.sleepynate.adventofcode.Day8
import org.scalatest.{FlatSpec, Matchers}

class Day8Test extends FlatSpec with Matchers {

  "rect" should "make a rect" in {
    new Day8 {
      val expected: Set[Pixel] = Set("0,0", "1,0", "2,0",
                                     "0,1", "1,1", "2,1")
      (Screen(7, 3) -> Rect(3, 2)).onPixels should be (expected)
    }
  }

  "rotate column" should "rotate a column" in {
    new Day8 {
      val expected:Set[Pixel] = Set("0,0", /* */  "2,0",
                                    "0,1", "1,1", "2,1",
                                    /* */  "1,2")
      (Screen(7, 3) ->
        Rect(3, 2) ->
        Column(1, 1)).onPixels should be (expected)
    }
  }

  "rotate row" should "rotate a row" in {
    new Day8 {
      val expected:Set[Pixel] = Set(/* */  /* */  /* */  /* */ "4,0", /* */ "6,0",
                                    "0,1", "1,1", "2,1",
                                    /* */  "1,2")
      (Screen(7, 3) ->
        Rect(3, 2) ->
        Column(1, 1) ->
        Row(0, 4)).onPixels should be (expected)
    }
  }

  "rotate column again" should "rotate a column again" in {
    new Day8 {
      val expected:Set[Pixel] = Set(/* */  "1,0",  /* */  /* */ "4,0", /* */ "6,0",
                                    "0,1", /* */  "2,1",
                                    /* */  "1,2")
      (Screen(7, 3) ->
        Rect(3,2) ->
        Column(1,1) ->
        Row(0,4) ->
        Column(1,1)).onPixels should be(expected)
    }
  }

  "parseInstruction" should "parse instructions correctly" in {
    new Day8 {
      parseInstruction("rect 1x1") should be (Rect(1,1))
      parseInstruction("rotate row y=0 by 5") should be (Row(0,5))
      parseInstruction("rotate column x=0 by 1") should be(Column(0,1))
    }
  }

  "part 1 solution" should "give the solution" in {
    new Day8 {
      val initial = Screen(50, 6)
      val res = getInput.map(parseInstruction).foldLeft(initial) {
        (s, i) => s -> i
      }
      println(res)
      res.onPixels.size should be (0)
    }
  }
}
