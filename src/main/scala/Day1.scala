import scala.io.Source

object Day1 {
 import FileParser._

  def main(args: Array[String]) = {
    val ints = parse("day1.txt", _.toInt)
    val intsGroupedBy3Sliding = ints.sliding(3).map(_.sum).toSeq

    val sumOfIncreasingValues = sumIncreasedFromPrevious(ints)
    val sumOfIncreasingValuesSliding3 = sumIncreasedFromPrevious(intsGroupedBy3Sliding)

    println(sumOfIncreasingValues)
    println(sumOfIncreasingValuesSliding3)

    println(sumList(ints.tail, ints.head, 0))
    println(sumList(intsGroupedBy3Sliding.tail, intsGroupedBy3Sliding.head, 0))
  }

  def sumIncreasedFromPrevious(list:Seq[Int]): Int = {
   list.tail.foldLeft((0,list.head))(accIfPreviousIncreased)._1
  }

  def accIfPreviousIncreased( tup:(Int, Int), currInt:Int): (Int, Int) = {
    val (acc, oldInt) = tup
    if(currInt > oldInt) (acc+1, currInt)
    else (acc, currInt)
  }

  def sumList(list:Seq[Int], previous:Int, acc:Int):Int = {
    if(list.isEmpty) acc
    else if(list.head > previous) sumList(list.tail, list.head, acc+1)
    else sumList(list.tail, list.head, acc)
  }
}
