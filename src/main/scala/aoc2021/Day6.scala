package aoc2021

import utils.FileParser._
object Day6 {
    val AddOnNewFish=2
    val ReproductionTime=6

    def reproductionCycle(days: Int, fishies: Map[Int, Long]): Map[Int, Long] = {
        if(days == 0) fishies
        else {
            val fishMap = fishies.foldLeft(Map.empty[Int, Long]) { (fishMap, u) =>
                val (reproductionTime, nrOfFish) = u
                if(reproductionTime-1 < 0) {
                    fishMap +-> (ReproductionTime + AddOnNewFish, nrOfFish) +-> (ReproductionTime, nrOfFish)
                } else fishMap +-> (reproductionTime-1 -> nrOfFish)
            }
            reproductionCycle(days-1, fishMap)
        }
    }

    implicit class MapWithCreateOrUpdate(map: Map[Int, Long]) {
        def +->(entry: (Int, Long)): Map[Int, Long] = map + (entry._1 -> (map.get(entry._1).getOrElse(0L) + entry._2))
    }

    def taskOne(fishies: Map[Int, Long]): Long = reproductionCycle(80, fishies).values.sum
    def taskTwo(fishies: Map[Int, Long]): Long = reproductionCycle(256, fishies).values.sum

    def main(args: Array[String]) = {
        val input = parse("aoc2021/Day6.txt", l => l).head
        val fishMap = input.split(",").toSeq.map(_.toInt).groupBy(identity).map(t => (t._1, t._2.size.toLong))
        println(taskOne(fishMap))
        println(taskTwo(fishMap))
    }
}
