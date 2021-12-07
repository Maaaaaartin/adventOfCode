package aoc2021
import utils.FileParser._
object Day5 {
  case class Cord(x: Int, y: Int)

  case class Line(from: Cord, to: Cord) {
    def isVertical = from.x == to.x

    def isHorizontal = from.y == to.y

    def isStraight = isVertical || isHorizontal

    def points: Seq[Cord] = {
      if (isVertical) (from.y -> to.y).map(Cord(from.x, _))
      else if (isHorizontal) (from.x -> to.x).map(Cord(_, from.y))
      else (from.x -> to.x).zip((from.y -> to.y)).map(c => Cord(c._1, c._2))
    }
  }

  implicit class RangeNegative(start: Int) {
    def ->(end: Int): Range = {
      start to end by (if (start > end) -1 else 1)
    }
  }

  object Line {
    implicit def toi(str: String): Int = augmentString(str).toInt

    def apply(str: String): Line = {
      str match {
        case s"$x1,$y1 -> $x2,$y2" => Line(Cord(x1, y1), Cord(x2, y2))
      }
    }
  }

  def createMapOfCordVisits(lines: Seq[Line]): Map[Cord, Int] = {
    lines.flatMap(_.points).groupBy(identity).map(t => (t._1, t._2.size)) //.values
    //.count(_.size > 1)
    /*lines.flatMap(_.points).foldLeft(Map.empty[Cord, Int]) { (map, cord) =>
      if(map.contains(cord)) map + ( cord -> (map(cord)+1))
      else map + ( cord -> 1)
    }*/
  }

  def taskOne(lines: Seq[Line]): Int = {
    val cordVisits = createMapOfCordVisits(lines.filter(_.isStraight))
    cordVisits.filter(_._2 > 1).size
  }

  def taskTwo(lines: Seq[Line]): Int = {
    val cordVisits = createMapOfCordVisits(lines)
    cordVisits.filter(_._2 > 1).size
  }

  def main(args: Array[String]): Unit = {
    val lines = parse("aoc2021/Day5.txt", Line(_))
    println(taskOne(lines))
    println(taskTwo(lines))
  }
}
