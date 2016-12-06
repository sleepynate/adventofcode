package com.sleepynate.adventofcode

import com.sleepynate.adventofcode.util.PipeOps._

import scala.io.Source

trait Day4 {

  case class Room(name: String, sector: Int, checksum: String) {

    def validate:Boolean = {
      val sortedName = name.filter(_.isLetter).sorted.groupBy(c => c).values.toList.sortBy(a => (-a.length, a.head))

      def f(checksum: List[Char], name: List[String]): Boolean = {
        checksum match {
          case Nil => true
          case h :: t => if (name.head.charAt(0) == h) f(t, name.tail) else false
        }
      }

      f(checksum.toList, sortedName)
    }

    def decryptName: String = {
      name.toList.map(c => {
        if (c == '-') ' '
        else {
          val i = c + sector % 26
          if (i > 122) (i - 26).toChar
          else i.toChar
        }
      }).mkString
    }
  }

  case object Room {
    def fromString(s: String) = {
      val parts = s.split('-').groupBy(_.head.isLetter)
      val name = parts(true).mkString("-")
      val parts2 = parts(false).head.split('[')

      Room(name, parts2(0).toInt, parts2(1).filter(_ != ']'))
   }
  }


  def getInput:List[String] = (getClass.getResourceAsStream("/day4.txt") |>
                               Source.fromInputStream).getLines.toList

  def validateRoom(room: String) = {
    Room.fromString(room).validate
  }

}
