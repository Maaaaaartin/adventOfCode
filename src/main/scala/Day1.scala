import scala.io.Source

object Day1 {
  def parse[T](fileName:String, mapper:(String => T)):Seq[T] = {
    Source.fromFile(s"src/main/resources/$fileName").getLines().map(mapper).toSeq
  }

  def main(args: Array[String]) = {
    val ints = parse("day1.txt", _.toInt)
    val intsGroupedBy3Sliding = ints.sliding(3).map(_.sum).toSeq

    val sumOfIncreasingValues = sumIncreasedFromPrevious(ints)
    val sumOfIncreasingValuesSliding3 = sumIncreasedFromPrevious(intsGroupedBy3Sliding)

    println(sumOfIncreasingValues)
    println(sumOfIncreasingValuesSliding3)
  }

  def sumIncreasedFromPrevious(list:Seq[Int]): Int = {
   list.foldLeft((0,Option.empty[Int]))(accIfPreviousIncreased)._1
  }

  def accIfPreviousIncreased( tup:(Int, Option[Int]), currInt:Int): (Int, Option[Int]) = {
    val (acc, optInt) = tup
    optInt match {
      case Some(aInt) => if(currInt > aInt) (acc+1, Some(currInt)) else (acc, Some(currInt))
      case _ => (acc, Some(currInt))
    }
  }
}
