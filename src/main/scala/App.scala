import java.io.FileWriter

import scala.io.Source

object App {

  def main(args: Array[String]) = {
    val rangesBuffer = Source.fromFile("ranges.tsv")
    val ranges = rangesBuffer.getLines().foldLeft(List[Range]())((op: List[Range], current: String) => {
      new Range(current) :: op
    })
    rangesBuffer.close()
    val out = new FileWriter("output.tsv")
    val transactionsBuffer = Source.fromFile("transactions.tsv")
    transactionsBuffer.getLines().foreach(str => {
      val words = str.split("\t")
      val range = ranges.find(p => p.isInRange(words(1)))
      out.write(words(0) + "\t" + (range.exists(p => p != null) match {
        case true => range.get.name
        case false => "Range not exist"
      }) + "\n")
    })
    out.flush()
  }
}
