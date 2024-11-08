import scala.collection.mutable.ListBuffer

object PrimeFactors {
  def factors(num: Long): List[Long] =
    var n = num
    val fs = ListBuffer[Long]()

    val addFactors = (fact: Long) =>
      while n % fact == 0 do
        fs += fact
        n = n / fact
  
    addFactors(2)
    var f = 3
    while f * f <= n do
      addFactors(f)
      f = f + 2
    if n > 1 then fs += n
    fs.toList
}