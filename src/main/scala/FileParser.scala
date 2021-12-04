import scala.io.Source

object FileParser {
  def parse[T](fileName:String, mapper:(String => T)):Seq[T] = {
    Source.fromFile(s"src/main/resources/$fileName").getLines().map(mapper).toSeq
  }

  def parse[T,U](cls: Class[U], mapper:(String => T)):Seq[T] = {
    parse(cls.getSimpleName+".txt", mapper)
  }
}
