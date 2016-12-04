package com.sleepynate.adventofcode

import com.sleepynate.adventofcode.util.PipeOps._

import scala.collection.immutable.Seq
import scala.io.Source

trait Day3 {
  def getInput:List[String] = (getClass.getResourceAsStream("/day3.txt") |>
                                Source.fromInputStream).getLines.toList

  def splitInput(input: List[String]): Seq[Array[Int]] = {
    for (line <- input) yield line.split(" ").filter(_.nonEmpty).map(_.toInt)
  }

  def checkSides(sides: Seq[Int]) = {
    sides.combinations(2).forall(l => l.sum > sides.diff(l).head)
  }

  def transposeInput(triangles: Seq[Array[Int]]): Seq[Seq[Int]] = {
    triangles.transpose.map(_.grouped(3)).flatten
  }
}
