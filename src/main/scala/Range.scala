
class Range(range: String) {

  val words = range.split("[-\t]")
  val min = getNumericAddress(words(0))
  val max = getNumericAddress(words(1))
  val name = words(2)

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

  override def toString = min + "-" + max + " " + name
}
