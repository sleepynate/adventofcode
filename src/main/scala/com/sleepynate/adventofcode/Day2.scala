package com.sleepynate.adventofcode

import com.sleepynate.adventofcode.util.PipeOps._
import scala.io.Source

trait Day2 {

  abstract trait Key {
    def up:Key = this
    def right:Key = this
    def down:Key = this
    def left:Key = this
  }
  case object PhoneOne extends Key {
    override def right = PhoneTwo
    override def down = PhoneFour
  }
  case object PhoneTwo extends Key {
    override def right = PhoneThree
    override def down = PhoneFive
    override def left = PhoneOne
  }
  case object PhoneThree extends Key {
    override def down = PhoneSix
    override def left = PhoneTwo
  }
  case object PhoneFour extends Key {
    override def up = PhoneOne
    override def right = PhoneFive
    override def down = PhoneSeven
  }
  case object PhoneFive extends Key {
    override def up = PhoneTwo
    override def right = PhoneSix
    override def down = PhoneEight
    override def left = PhoneFour
  }
  case object PhoneSix extends Key {
    override def up = PhoneThree
    override def down = PhoneNine
    override def left = PhoneFive
  }
  case object PhoneSeven extends Key {
    override def up = PhoneFour
    override def right = PhoneEight
  }
  case object PhoneEight extends Key {
    override def up = PhoneFive
    override def right = PhoneNine
    override def left = PhoneSeven
  }
  case object PhoneNine extends Key {
    override def up = PhoneSix
    override def left = PhoneEight
  }
  case object PottyOne extends Key {
    override def down: Key = PottyThree
  }
  case object PottyTwo extends Key {
    override def right: Key = PottyThree
    override def down: Key = PottySix
  }
  case object PottyThree extends Key {
    override def up: Key = PottyOne
    override def right: Key = PottyFour
    override def down: Key = PottySeven
    override def left: Key = PottyTwo
  }
  case object PottyFour extends Key {
    override def down: Key = PottyEight
    override def left: Key = PottyThree
  }
  case object PottyFive extends Key {
    override def right: Key = PottySix
  }
  case object PottySix extends Key {
    override def up: Key = PottyTwo
    override def right: Key = PottySeven
    override def down: Key = PottyA
    override def left: Key = PottyFive
  }
  case object PottySeven extends Key {
    override def up: Key = PottyThree
    override def right: Key = PottyEight
    override def down: Key = PottyB
    override def left: Key = PottySix
  }
  case object PottyEight extends Key {
    override def up: Key = PottyFour
    override def right: Key = PottyNine
    override def down: Key = PottyC
    override def left: Key = PottySeven
  }
  case object PottyNine extends Key {
    override def left: Key = PottyEight
  }
  case object PottyA extends Key {
    override def up: Key = PottySix
    override def right: Key = PottyB
  }
  case object PottyB extends Key {
    override def up: Key = PottySeven
    override def right: Key = PottyC
    override def down: Key = PottyD
    override def left: Key = PottyA
  }
  case object PottyC extends Key {
    override def up: Key = PottyEight
    override def left: Key = PottyB
  }
  case object PottyD extends Key {
    override def up: Key = PottyB
  }

  def getInput:List[String] = (getClass.getResourceAsStream("/day2.txt") |>
                                Source.fromInputStream).getLines.toList

  def followInstructions(start: Key)(input: List[String]) =
    input.scanLeft(start) { case (k, s) => followInstructionsLine(k, s) }.tail

  def followInstructionsLine(starting: Key, instructions: String) = {
    instructions.foldLeft(starting) { (k: Key, c: Char) =>
      c match {
        case 'U' => k.up
        case 'R' => k.right
        case 'D' => k.down
        case 'L' => k.left
      }
    }
  }
}
