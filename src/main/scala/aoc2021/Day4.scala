package aoc2021

import scala.io.Source

object Day4 {
  case class BingoNumber(number: Int, crossed: Boolean) {
    private val formattedNumber = "%02d".format(number)

    def isNotCrossed = !crossed

    def cross(): BingoNumber = BingoNumber(number, true)

    override def toString() = if (crossed) s"(${formattedNumber})" else s" ${formattedNumber} "
  }

  case class Bingo(board: Seq[Seq[BingoNumber]], crossedNumbers: Seq[Int] = Seq.empty[Int]) {
    def crossNumber(nr: Int): Bingo = {
      Bingo(board.map(b => b.map((n => if (nr == n.number) n.cross else n))), crossedNumbers :+ nr)
    }

    def hasBingoOnColumn(): Boolean = board.transpose.exists(_.forall(_.crossed))

    def hasBingoOnRow(): Boolean = board.exists(_.forall(_.crossed))

    def hasBingo(): Boolean = hasBingoOnRow() || hasBingoOnColumn()

    def sumUncrossedNumbers() = board.flatten.filter(_.isNotCrossed).map(_.number).sum

    def score(): Int = crossedNumbers.reverse.head * sumUncrossedNumbers()

    override def toString() = board.map(nr => nr.mkString(" ")).mkString("\n") + "\n"
  }

  object Bingo {
    def apply(str: List[String]): Bingo = {
      val board: Seq[Seq[BingoNumber]] = str.map(_.grouped(3).map(nr => BingoNumber(nr.mkString.trim.toInt, false)).toSeq)
      Bingo(board)
    }
  }

  def taskOne(bingoBoards: Seq[Bingo]): Int = {
    val firstBingoBoard = bingoBoards.head
    firstBingoBoard.score()
  }

  def taskTwo(bingoBoards: Seq[Bingo]): Int = {
    val lastBingoBoard = bingoBoards.last
    lastBingoBoard.score()
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("src/main/resources/aoc2021/Day4.txt").getLines()
    val numbers = input.next().split(",").map(_.toInt).toSeq
    val boards = createBoards(input, Seq.empty[Bingo])
    val bingoBoards = crossNumbersUntilAllHasBingo(numbers, boards)

    println(taskOne(bingoBoards))
    println(taskTwo(bingoBoards))
  }

  def crossNumbersUntilAllHasBingo(numbers: Seq[Int], boards: Seq[Bingo], bingoBoards: Seq[Bingo] = Seq.empty[Bingo]): Seq[Bingo] = {
    if (numbers.isEmpty) bingoBoards
    else if (boards.isEmpty) bingoBoards
    else {
      val crossedNumberBoards = crossNumber(numbers.head, boards)
      crossNumbersUntilAllHasBingo(numbers.tail, crossedNumberBoards.filter(board => !board.hasBingo()), bingoBoards ++ crossedNumberBoards.filter(_.hasBingo()))
    }
  }

  def crossNumber(number: Int, boards: Seq[Bingo]): Seq[Bingo] = {
    boards.map(b => b.crossNumber(number))
  }

  def createBoards(input: Iterator[String], res: Seq[Bingo]): Seq[Bingo] = {
    if (input.isEmpty) res
    else {
      val bingo = Bingo(input.take(6).toList.tail)
      createBoards(input, res :+ bingo)
    }
  }
}
