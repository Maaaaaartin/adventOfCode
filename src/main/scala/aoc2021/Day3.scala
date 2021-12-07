package aoc2021
import utils.FileParser._
object Day3 {
  type Bits = Seq[Bit]
  type CountedBits = Seq[Map[Bit, Int]]
  type Criteria = Map[Bit, Int] => Bit

  object Bit {
    def apply(char: Char): Bit = {
      char match {
        case '0' => ZERO(char)
        case '1' => ONE(char)
        case _ => throw new IllegalArgumentException("Input is not a Bit")
      }
    }
  }

  abstract class Bit {
    def char: Char

    override def toString: String = char.toString
  }

  case class ONE(char: Char) extends Bit

  case class ZERO(char: Char) extends Bit

  implicit class toIntFromBinary(str: String) {
    def fromBinaryToInt: Int = {
      Integer.parseInt(str, 2)
    }
  }

  def taskOne(countedBits: Seq[Map[Bit, Int]]): Int = {
    val gammarate = countedBits.map(_.maxBy(_._2)._1).mkString.fromBinaryToInt
    val epsilonRate = countedBits.map(_.minBy(_._2)._1).mkString.fromBinaryToInt
    gammarate * epsilonRate
  }

  def co2ScrubberRatingRule(map: Map[Bit, Int]): Bit = if (map(Bit('0')) <= map(Bit('1'))) Bit('0') else Bit('1')

  def oxygenGeneratorRule(map: Map[Bit, Int]): Bit = if (map(Bit('1')) >= map(Bit('0'))) Bit('1') else Bit('0')

  def bitRuleApplies(bitList: Seq[Bits], res: Bits, criteria: Criteria): Bits = {
    if (bitList.size == 1) res ++ bitList.head
    else {
      val countedBits = countBits(bitList)
      val currBit = criteria(countedBits.head)
      bitRuleApplies(
        bitList.filter(_.head == currBit).map(_.tail), res :+ currBit, criteria)
    }
  }

  def taskTwo(bitList: Seq[Bits]): Int = {
    val co2ScrubberRating = bitRuleApplies(bitList, Seq.empty[Bit], co2ScrubberRatingRule).mkString.fromBinaryToInt
    val oxygenGenerator = bitRuleApplies(bitList, Seq.empty[Bit], oxygenGeneratorRule).mkString.fromBinaryToInt
    co2ScrubberRating * oxygenGenerator
  }

  def countBits(bits: Seq[Bits]): Seq[Map[Bit, Int]] = {
    bits.transpose.map(_.groupBy(c => c).map(t => (t._1, t._2.size)))
  }

  def main(args: Array[String]) = {
    val bits = parse("aoc2021/Day3.txt", _.map(Bit(_)))
    val countedBits = countBits(bits)

    println(taskOne(countedBits))
    println(taskTwo(bits))
  }

}
