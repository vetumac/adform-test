import java.io.{FileWriter, OutputStreamWriter}

import scala.io.Source

object App {

  def main(args: Array[String]) = {
    val ranges = getRanges("ranges.tsv")

    val output = new FileWriter("output.tsv")
    val input = Source.fromFile("transactions.tsv")

    transactionsMapping(input, output, ranges)

    input.close()
    output.flush()
  }

  def getRanges(file: String) = {
    val rangesBuffer = Source.fromFile(file)
    val ranges = rangesBuffer.getLines().foldLeft(List[Range]())((op: List[Range], current: String) => {
      op.foldLeft((List[Range](), Range(current)))((f: (List[Range], Range), current: Range) => {
        val confluenceRange = Range.confluenceRanges(current, f._2)
        confluenceRange match {
          case null => (current :: f._1, f._2)
          case _ => (f._1, confluenceRange);
        }
      })._1
    })
    rangesBuffer.close()
    ranges
  }

  def transactionsMapping(source: Source, destination: OutputStreamWriter, ranges: List[Range]) = {
    source.getLines().foreach(str => {
      val words = str.split("\t")
      val range = ranges.find(p => p.isInRange(Range.getNumericAddress(words(1))))
      destination.write(words(0) + "\t" + (range.exists(p => p != null) match {
        case true => range.get.name
        case false => "Range not exist"
      }) + "\n")
    })
  }
}
