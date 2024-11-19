import scala.collection.mutable.ListBuffer
import scala.util.boundary, boundary.break

object NthPrime {
  def prime(n: Int): Option[Int] =
    if n < 1 then
      None
    else
      while primes.length < n do primes.addOne(nextPrime())
      Some(primes.apply(n - 1))

  private val primes: ListBuffer[Int] = ListBuffer(2, 3)

  private def nextPrime(): Int =
    var p = primes.last
    while { p += 2; ! isPrime(p) } do ()
    p

  private def isPrime(n: Int): Boolean =
    boundary:
      for p <- primes do
        if p * p > n then break(true)
        if n % p == 0 then break(false)
      true
}


/* Notes:
 * https://dotty.epfl.ch/api/scala/util/boundary$.html
 * https://docs.scala-lang.org/scala3/guides/migration/incompat-dropped-features.html#do-while-construct
 */
