import scala.math.floor
import scala.math.log10
import scala.math.pow
import scala.collection.mutable.ListBuffer

object ArmstrongNumbers {
  def isArmstrongNumber(n: Int): Boolean = n == n.armstrongSum
}

extension (n: Int)
  def width: Int = 1 + (if (n == 0) then 0 else floor(log10(n)).toInt)

  def digits: List[Int] =
    var m = n
    val ds = ListBuffer[Int]()
    while m > 0 do
      ds += m % 10
      m = m / 10
    ds.toList
    
  def armstrongSum: Int =
    val w = n.width
    n.digits.map {pow(_, w).toInt}.sum
