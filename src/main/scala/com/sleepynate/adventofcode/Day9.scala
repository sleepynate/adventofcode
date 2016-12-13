package com.sleepynate.adventofcode

import scala.io.Source
import com.sleepynate.adventofcode.util.PipeOps._

trait Day9 {

  def getInput = (getClass.getResourceAsStream("/day9.txt") |> Source.fromInputStream).mkString.init

  def decompress(chars: String, v2: Boolean):Long = {
    val regex = """\((\d+)x(\d+)\)""".r

    def f(chars: String, acc: Long):Long = {
      regex.findFirstMatchIn(chars).map { m =>
        (m.start, m.end, m.group(1).toInt, m.group(2).toInt)
      } match {
        case None => acc + chars.length
        case Some((start, end, span, repitions)) => {
          val (toReplicate, tail) = chars.substring(end.toInt).splitAt(span)
          if (v2) {
            val decompressed = f(toReplicate, 0) * repitions
            f(tail, acc + chars.substring(0, start).length + decompressed)
          }
          else {
            val decompressed = toReplicate.length * repitions
            f(tail, acc + chars.substring(0, start).length + decompressed)
          }
        }
      }
    }
    f(chars, 0)
  }

}
