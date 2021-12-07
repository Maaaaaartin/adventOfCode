package utils

import scala.io.Source

object FileParser {
  def parse[T](fileName: String, mapper: (String => T)): Seq[T] = {
    Source.fromFile(s"src/main/resources/$fileName").getLines().map(mapper).toSeq
  }
}
