package com.sleepynate.adventofcode

import scala.io.Source
import com.sleepynate.adventofcode.util.PipeOps._

trait Day10 {

  sealed trait ChipReceiver { val n: Int }
  case class OutputR(n: Int) extends ChipReceiver
  case class BotR(n: Int) extends ChipReceiver

  sealed trait Instruction
  case class Value(n: Int, bot: Int) extends Instruction
  case class Give(source: Int, lowTo: ChipReceiver, highTo: ChipReceiver) extends Instruction

  sealed trait ChipHolder
  case class Bot(number: Int, chips: Seq[Int]) extends ChipHolder
  case class Output(number: Int, chips: Seq[Int]) extends ChipHolder

  case class BotState(bots: Map[Int, Bot], bins: Map[Int,Seq[Int]]) {

    def addChipToBot(chip: Int, bot: Int): BotState = {
      if (!bots.contains(bot)) {
        copy(bots = bots + (bot -> Bot(bot, Seq(chip))))
      } else {
        val oldBot = bots(bot)
        copy(bots = bots - bot + (bot -> oldBot.copy(chips=oldBot.chips :+ chip)))
      }
    }

    def addChipToBin(chip: Int, bin: Int): BotState = {
      if (!bins.contains(bin)) {
        copy(bins = bins + (bin -> Seq(chip)))
      } else {
        val oldBin = bins(bin)
        copy(bins = bins - bin + (bin -> (oldBin :+ chip)))
      }
    }

    def receiveChip(cr: ChipReceiver, chip: Int) = cr match {
      case BotR(n) => addChipToBot(chip, n)
      case OutputR(n) => addChipToBin(chip, n)
    }

    def dropBot(n: Int) = copy(bots = bots - n)

    def canPerform(instruction: Instruction): Boolean = instruction match {
      case Value(n, bot) => true
      case Give(source, _, _) => bots.contains(source) && bots(source).chips.length == 2
    }

    def perform(instruction: Instruction):BotState = instruction match {
      case Value(n, bot) => addChipToBot(n, bot)
      case Give(source, lowTo, highTo) =>
        val oldBot = bots(source)
        val lowChip = oldBot.chips.min
        val highChip = oldBot.chips.max
        if (lowChip == 17 && highChip == 61) {
          println("target comparison made by bot " + oldBot)
        }
        val withHigh = receiveChip(lowTo, lowChip).receiveChip(highTo, highChip)
        withHigh.dropBot(source)
    }
  }

  val valueRegex = """value (\d+) goes to bot (\d+)""".r
  val giveRegex = """bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)""".r

  def parseReceiver(receiverType: String, receiverValue: String) = {
    if(receiverType == "output") {
      OutputR(receiverValue.toInt)
    } else {
      BotR(receiverValue.toInt)
    }
  }

  def parseInstruction(s: String):Instruction = {
    if(s(0) == 'v') { //valueCommand
      val valueRegex(chipValue, destinationNumber) = s
      Value(chipValue.toInt, destinationNumber.toInt)
    } else {
      val giveRegex(bot, lowType, low, highType, high) = s
      Give(bot.toInt, parseReceiver(lowType, low), parseReceiver(highType, high))
    }
  }

  def doTheThing(instructions: List[String]):BotState = {
    def f(instructions: List[String], acc: BotState):BotState = {
      instructions match {
        case Nil => acc
        case i :: is =>
          val instruction = parseInstruction(i)
          if (acc.canPerform(instruction)) {
            f(is, acc.perform(instruction))
          } else {
            f(is :+ i, acc)
          }
      }

    }
    f(instructions, BotState(Map.empty[Int, Bot], Map.empty[Int, Seq[Int]]))
  }

  def getInput = (getClass.getResourceAsStream("/day10.txt") |> Source.fromInputStream).getLines.toList
}
