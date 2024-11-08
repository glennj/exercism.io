import scala.annotation.tailrec

object CollatzConjecture {
  @tailrec
  def steps(n: Int, step: Int = 0): Option[Int] =
    n match
      case 1                  => Some(step)
      case _ if !n.isPositive => None
      case _ if n.isEven      => steps(n / 2, step + 1)
      case _                  => steps(3 * n + 1, step + 1)
}

extension (n: Int)
  def isEven: Boolean = (n & 1) == 0
  def isPositive: Boolean = n > 0
