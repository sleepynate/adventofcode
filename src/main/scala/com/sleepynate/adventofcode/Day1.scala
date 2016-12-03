package com.sleepynate.adventofcode
import util.PipeOps._
import scala.io.Source

trait Day1 {
  trait Turn
  case object Right extends Turn
  case object Left  extends Turn

  type Blocks = Int

  type Direction = (Turn, Blocks)

  trait Bearing
  case object North extends Bearing
  case object East  extends Bearing
  case object South extends Bearing
  case object West  extends Bearing

  case class Location(x: Int, y: Int, bearing: Bearing) {

    def move(b: Blocks): Location = {
      bearing match {
        case North => copy(y = y + b)
        case East  => copy(x = x + b)
        case South => copy(y = y - b)
        case West  => copy(x = x - b)
      }
    }

    def turn(turn: Turn): Location = {
      bearing match {
        case North => turn match {
          case Left  => copy(bearing = West)
          case Right => copy(bearing = East)
        }
        case East => turn match {
          case Left  => copy(bearing = North)
          case Right => copy(bearing = South)
        }
        case South => turn match {
          case Left  => copy(bearing = East)
          case Right => copy(bearing = West)
        }
        case West => turn match {
          case Left  => copy(bearing = South)
          case Right => copy(bearing = North)
        }
      }
    }

    def go(d: Direction): Location = {
      turn(d._1).move(d._2)
    }
  }

  def getInput:String = Source fromInputStream getClass.getResourceAsStream("/day1.txt") mkString

  def parseThatInputYo(input: String): Array[String] = {
    input split ',' map (s => s.trim)
  }

  def parseDirection(d: String):Direction = {
    val turn = d.head
    val move = d.tail
    turn match {
      case 'R' => (Right, move.toInt)
      case 'L' => (Left,  move.toInt)
    }
  }

  case class LocationHistory(current: Location, history: List[Location]) {
    def add(location: Location) = {
      if (history.exists(l => l.x == location.x && l.y == location.y)) println(s"duplicate at ${location}")
      else println(s"adding new point ${location}")
      copy(current = location, history = history :+ current)
    }
  }

  def followDirections(directions: Array[String]):LocationHistory = {
    val start = LocationHistory(Location(0, 0, North), List.empty[Location])
    directions.foldLeft(start) {
      (history: LocationHistory, direction:String) =>
        val location = direction |> parseDirection |> history.current.go
        println(location)
        history.add(location)
    }
  }

  def distanceFromOrigin(location: Location) = math.abs(location.x) + math.abs(location.y)
}
