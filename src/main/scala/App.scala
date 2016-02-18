import scala.io.Source

object App {

  def main(args: Array[String]) = {
    val transactionsBuffer = Source.fromFile("transactions.tsv") //main/resources/transactions.tsv
    transactionsBuffer.foreach(f => print(f))
  }
}
