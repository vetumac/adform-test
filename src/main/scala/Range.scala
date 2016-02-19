
class Range(val min: Int, val max: Int, val name: String) {

  def isInRange(address: Int) = address >= min && address <= max

  override def toString = min + "-" + max + " " + name
}

object Range {
  def apply(range: String) = {
    val words = range.split("[-\t]")
    val min = getNumericAddress(words(0))
    val max = getNumericAddress(words(1))
    val name = words(2)
    new Range(min, max, name)
  }

  def getNumericAddress(address: String): Int = {
    val addr = address.split("[.]")
    addr(3).toInt +
      (addr(2).toInt << 8) +
      (addr(1).toInt << 16) +
      (addr(1).toInt << 32)
  }

  def confluenceRanges(a: Range, b: Range): Range = {
    val lst = a.min > b.min match {
      case true => List(b, a)
      case false => List(a, b)
    }
    lst(0).max < lst(0).min match {
      case true => null
      case false => lst(0).max < lst(1).max match {
        case true => new Range(lst(0).min, lst(1).max, lst(0).name)
        case false => lst(0)
      }
    }
  }
}
