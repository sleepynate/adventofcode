package com.sleepynate.adventofcode

import com.sleepynate.adventofcode.util.PipeOps._
import scala.io.Source

trait Day6 {
  def getInput:List[String] = (getClass.getResourceAsStream("/day6.txt") |>
    Source.fromInputStream).getLines.toList

  def transpose(input: List[String]): List[List[Char]] = {
    input.map(_.toList).transpose
  }

  def mostCommonInRow(matrix: List[List[Char]]) = {
    matrix.map(_.groupBy(identity).maxBy(_._2.size)._1)
  }
  def leastCommonInRow(matrix: List[List[Char]]) = {
    matrix.map(_.groupBy(identity).minBy(_._2.size)._1)
  }
}
