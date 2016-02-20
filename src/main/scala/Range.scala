
class Range(val min: Long, val max: Long, val name: String) {

  def isInRange(address: Long) = address >= min && address <= max

  override def toString = Range.getStringAddress(min) + "-" + Range.getStringAddress(max) + "\t" + name
}

object Range {
  def apply(range: String) = {
    val words = range.split("[-\t]")
    val min = getNumericAddress(words(0))
    val max = getNumericAddress(words(1))
    val name = words(2)
    new Range(min, max, name)
  }

  def getNumericAddress(address: String) = {
    val addr = address.split("[.]")
    addr(3).toLong +
      (addr(2).toLong << 8) +
      (addr(1).toLong << 16) +
      (addr(0).toLong << 32)
  }


  def getStringAddress(num: Long) = {
    (num >> 32 & 0xFF) + "." +
      (num >> 16 & 0xFF) + "." +
      (num >> 8 & 0xFF) + "." +
      (num & 0xFF)
  }

  def confluenceRanges(a: Range, b: Range): Range = {
    a.name == b.name match {
      case true =>
        val lst = a.min > b.min match {
          case true => List(b, a)
          case false => List(a, b)
        }
        lst.head.max < lst(1).min match {
          case true => null
          case false => lst.head.max < lst(1).max match {
            case true => new Range(lst.head.min, lst(1).max, lst.head.name)
            case false => lst.head
          }
        }
      case false => null
    }
  }
}
