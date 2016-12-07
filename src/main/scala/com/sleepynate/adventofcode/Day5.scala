package com.sleepynate.adventofcode

import java.security.MessageDigest

trait Day5 {
  def md5(text: String) = {
    MessageDigest.getInstance("MD5").digest(text.getBytes).map("%02x".format(_)).mkString
  }

  def haystack(seed: String) = Stream.from(1).map(seed + _.toString)

  def zeroHashes(seed: String, p: String => Boolean) = haystack(seed).map(md5).filter(p)
//    haystack(seed).collect { case x if p(md5(x)) => md5(x) }

  def foldUntil8(stream: Stream[String]):List[(Char, Char)] = {
    stream.foldLeft(List.empty[(Char, Char)]) { (c, n) =>
      if(c.length >= 8) return c
      val index = n.charAt(5)
      if (c.map(_._1).contains(index)) {
        c
      } else {
        (index, n.charAt(6)) :: c
      }
    }
  }
}
