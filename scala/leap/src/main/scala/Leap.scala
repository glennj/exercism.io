// an extension method for an int: does the given factor f divide integer n evenly?
extension (n: Int)
  def div(f: Int): Boolean = n % f == 0

object Leap {
  def leapYear(year: Int): Boolean = year.div(4) && (!year.div(100) || year.div(400))
}
