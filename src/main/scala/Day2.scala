import scala.collection.StringOps

object Day2 {
  import FileParser._
  abstract class Direction()
  case class Forward() extends Direction
  case class Up() extends Direction
  case class Down() extends Direction
  case class Command(direction:Direction, steps: Int)

  object Direction {
    def apply(str: String): Direction =
      str match {
        case "forward" => Forward()
        case "down" => Down()
        case "up" => Up()
      }
  }

  object Command {
    def apply(str:String): Command = {
      str match {
        case s"$direction $steps" => Command(Direction(direction), steps.toInt)
      }
    }
  }

  def taskOne(commands: Seq[Command]): Int = {
    val (depth,horizontal) = commands.foldLeft((0, 0)) ( (p, command) => {
      val (depth, horizontal) = p
      command match {
        case Command(Forward(), steps) => (depth, horizontal+steps)
        case Command(Up(), steps) => (depth-steps, horizontal)
        case Command(Down(), steps) => (depth+steps, horizontal)
      }
    })
    depth*horizontal
  }

  def taskTwo(commands:Seq[Command]): Int = {
    val (depth,horizontal, aim) = commands.foldLeft((0, 0, 0)) ( (p, command) => {
      val (depth, horizontal, aim) = p
      command match {
        case Command(Forward(), steps) => (depth + aim*steps, horizontal+steps, aim)
        case Command(Up(), steps) => (depth, horizontal, aim-steps)
        case Command(Down(), steps) => (depth, horizontal, aim+steps)
      }
    })
    depth*horizontal
  }

  def main(args: Array[String]): Unit = {
    val commands = parse("Day2.txt", Command(_))
    println("Task 1:" + taskOne(commands))
    println("Task 2:" + taskTwo(commands))
  }
}
