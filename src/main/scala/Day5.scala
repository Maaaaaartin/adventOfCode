import FileParser._

object Day5 {
  case class Cord(x: Int, y: Int)
  case class Line(from: Cord, to: Cord)  {
    def isVertical = from.x == to.x
    def isHorizontal = from.y == to.y
    def isStraight = isVertical || isHorizontal
    def points: Seq[Cord] = {
      if(isVertical) range(from.y, to.y).map(Cord(from.x, _))
      else if(isHorizontal) range(from.x, to.x).map(Cord(_, from.y))
      else range(from.x, to.x).zip(range(from.y, to.y)).map(c => Cord(c._1, c._2))
    }
  }

  def range(start: Int, end: Int) = {
    if(start >= end) start to end by -1
    else start to end
  }

  object Line {
    def apply(str: String): Line = {
      str match {
        case s"$x1,$y1 -> $x2,$y2" => Line(Cord(x1.toInt, y1.toInt), Cord(x2.toInt, y2.toInt))
      }
    }
  }

  def createMapOfCordVisits(lines: Seq[Line]): Map[Cord, Int] = {
    lines.flatMap(_.points).foldLeft(Map.empty[Cord, Int]) { (map, cord) =>
      if(map.contains(cord)) map + ( cord -> (map(cord)+1))
      else map + ( cord -> 1)
    }
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
    val lines = parse("Day5.txt", Line(_))
    println(taskOne(lines))
    println(taskTwo(lines))
  }
}
