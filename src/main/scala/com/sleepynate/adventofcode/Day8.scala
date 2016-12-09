package com.sleepynate.adventofcode

import scala.collection.immutable.Seq
import scala.io.Source
import scala.language.implicitConversions

import com.sleepynate.adventofcode.util.PipeOps._

trait Day8 {

  def getInput:List[String] = (getClass.getResourceAsStream("/day8.txt") |>
                                Source.fromInputStream).getLines.toList

  trait Instruction
  sealed case class Rect(x:Int, y:Int) extends Instruction
  sealed case class Column(x:Int, y:Int) extends Instruction
  sealed case class Row(x:Int, y:Int) extends Instruction

  case class Pixel (x: Int, y:Int)

  implicit def string2Pixel(s: String):Pixel = { val coords = s.split(','); Pixel(coords(0).toInt, coords(1).toInt) }

  case class Screen(x: Int, y: Int, onPixels: Set[Pixel] = Set.empty[Pixel]) {

    def ->(i: Instruction): Screen = i match {
      case Rect(a, b) => rect(a, b)
      case Column(a, b) => rotateCol(a, b)
      case Row(a, b) => rotateRow(a, b)
    }

    def rect(x: Int, y: Int):Screen = {
      val pixels: Seq[Pixel] = for (xs <- 0 until x; ys <- 0 until y) yield Pixel(xs, ys)
      copy(onPixels = onPixels.union(pixels.toSet))
    }

    def rotateCol(column: Int, distance: Int):Screen = {
      transform(_.x == column, p => p copy (y = (p.y + distance) % y))
    }

    def rotateRow(row: Int, distance: Int):Screen = {
      transform(_.y == row, p => p copy (x = (p.x + distance) % x))
    }

    private def transform(f: (Pixel) => Boolean, g: (Pixel) => Pixel):Screen = {
      val pixelsToChange = onPixels filter (f)
      val newPixels = pixelsToChange map (g)
      copy(onPixels=onPixels &~ pixelsToChange | newPixels)
    }

    override def toString: String = {
      (0 until y).map { row =>
        (0 until x).map { col =>
          if (onPixels.contains(Pixel(col, row))) 182.toChar
          else ' '
        }.mkString
      }.mkString("\n")
    }
  }

  def parseInstruction(s: String):Instruction = {
    if (s.startsWith("rect")) {
      val coords = s.drop(5).split('x')
      Rect(coords(0).toInt, coords(1).toInt)
    } else if (s.startsWith("rotate column")) {
      val coords = s.drop(16).split(" by ")
      Column(coords(0).toInt, coords(1).toInt)
    } else if (s.startsWith("rotate row")) {
      val coords = s.drop(13).split(" by ")
      Row(coords(0).toInt, coords(1).toInt)
    } else {
      throw new UnsupportedOperationException
    }
  }
}
