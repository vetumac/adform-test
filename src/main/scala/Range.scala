
class Range(val min: Int, val max: Int, val name: String) {

  def this(range: String) {
    this(min, max, name)
    val words = range.split("[-\t]")
    val min = getNumericAddress(words(0))
    val max = getNumericAddress(words(1))
    val name = words(2)
  }

  private def getNumericAddress(address: String): Int = {

    val addr = address.split("[.]")

    addr(3).toInt +
      (addr(2).toInt << 8) +
      (addr(1).toInt << 16) +
      (addr(1).toInt << 32)
  }

  def isInRange(addr: String) = {
    val address = getNumericAddress(addr)
    address >= min && address <= max
  }

  def confluenceRanges(a: Range, b: Range): Unit = {

    val lst = a.min > b.min match {
      case true => List(b, a)
      case false => List(a, b)
    }
    lst(0).max < lst(0).min match {
      case true => null
      case false => lst(0).max < lst(1).max match {
      new Range
      }
    }
  }

  override def toString = min + "-" + max + " " + name
}
