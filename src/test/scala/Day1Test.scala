package com.sleepynate.adventofcode.test

import com.sleepynate.adventofcode.Day1
import com.sleepynate.adventofcode.util.PipeOps._
import org.scalatest.{FlatSpec, Matchers}

class Day1Test extends FlatSpec with Matchers {

  "Day1" should "be able to find its resources" in {
    new Day1 {
      val input = getInput
      input should have size 633
    }
  }

  "input" should "be split from comma-separated" in {
    new Day1 {
      val parsed = parseThatInputYo("1, 2, 3")
      parsed should have size 3
      parsed.head should be ("1")
    }
  }

  "one move" should "provide a location" in {
    new Day1 { val directions = parseThatInputYo("R2")
      val loc = followDirections(directions)
      loc.current.x should be(2)
      loc.current.y should be(0)
    }
  }

  "parseDirections" should "not be dumb" in {
    new Day1 {
      val direction = parseDirection("R2")
      direction._1 should be(Right)
      direction._2 should be(2)
    }
  }

  "confusing directions" should "calculate correctly anyways" in {
    new Day1 {
      val loc = followDirections(parseThatInputYo("R2, R2, R2"))
      loc.current.x should be(0)
      loc.current.y should be(-2)
    }
  }

  "longer directions" should "give a distance in blocks" in {
    new Day1 {
      val loc = followDirections(parseThatInputYo("R5, L5, R5, R3"))
      distanceFromOrigin(loc.current) should be (12)
    }
  }

  "step one solution" should "be correct i guess" in {
    new Day1 {
      val distance = getInput |>
                     parseThatInputYo |>
                     followDirections |>
                     ((h: LocationHistory) => h.current) |>
                     distanceFromOrigin
      distance should be(5) // not giving away answers
    }
  }

  "a circular history" should "be 0 distance for first point" in {
    new Day1 {
      val h = ("R4, R4, R4, R4, L4, L4, R4" |> parseThatInputYo |> followDirections).history
      val loc = h.collectFirst {
        case l if h.count(m => m.x == l.x && m.y == l.y) > 1 => l
      }
      loc.get.x should be (0)
      loc.get.y should be (0)
    }
  }

  "step two solution" should "be a real pain in the butt" in {
    new Day1 {
      val history = (getInput |> parseThatInputYo |> followDirections) history
      val dupes = history.collectFirst {
        case l if history.count(m => m.x == l.x && m.y == l.y) > 1 => l
      }
      val distance = distanceFromOrigin(dupes.get)
      distance should be (5) // no giving away answers
    }
  }
}
