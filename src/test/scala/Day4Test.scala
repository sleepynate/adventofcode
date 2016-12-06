package com.sleepynate.adventofcode.test

import com.sleepynate.adventofcode.Day4
import com.sleepynate.adventofcode.util.PipeOps._
import org.scalatest.{FlatSpec, Matchers}
class Day4Test  extends FlatSpec with Matchers {

  "input" should "uh, exist" in {
    new Day4 {
      getInput should have size(1091)
    }
  }

  "rooms names" should "get validated" in {
    new Day4 {
      ("aaaaa-bbb-z-y-x-123[abxyz]"   |> validateRoom) should be(true)
      ("a-b-c-d-e-f-g-h-987[abcde]"   |> validateRoom) should be(true)
      ("not-a-real-room-404[oarel]"   |> validateRoom) should be(true)
      ("totally-real-room-200[decoy]" |> validateRoom) should be(false)
    }
  }

  "parsing rooms" should "work correctly" in {
    new Day4 {
      val parsed = Room.fromString("aaaaa-bbb-z-y-x-123[abxyz]")
      parsed.name should be ("aaaaa-bbb-z-y-x")
      parsed.sector should be (123)
      parsed.checksum should be ("abxyz")

      val parsed2 = Room.fromString("a-b-c-d-e-f-g-h-987[abcde]")
      parsed2.name should be ("a-b-c-d-e-f-g-h")
      parsed2.sector should be (987)
      parsed2.checksum should be ("abcde")
    }
  }

  "sample solition" should "be correct" in {
    new Day4{
      val sum = List("aaaaa-bbb-z-y-x-123[abxyz]",
        "a-b-c-d-e-f-g-h-987[abcde]",
        "not-a-real-room-404[oarel]",
        "totally-real-room-200[decoy]").map(Room.fromString).foldLeft(0) {
        (c, n) => if (n.validate) c+n.sector else c
      }
      sum should be (1514)
    }
  }

  "part one solution" should "validate that ridiculous match function" ignore {
    new Day4  {
      val goodRooms = getInput.collect {
        case s if (s |> Room.fromString).validate => Room.fromString(s).sector
      }
      val sum = goodRooms.reduce(_+_)
      sum should be (0)
    }
  }

  "decrypting names" should "shift cipher it up" in {
    new Day4 {
      val room = Room.fromString("qzmt-zixmtkozy-ivhz-343[derp]")
      room.decryptName should be ("very encrypted name")
    }
  }

  "part 2 solution" should "find north pole objects" ignore {
    new Day4 {
      val rooms = getInput.map(Room.fromString)
      val decrypted = rooms.filter(r => r.validate && r.decryptName.startsWith("northpole"))
      decrypted.head.sector should be (0)
    }
  }
}
