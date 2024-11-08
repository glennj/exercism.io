object Grains {
  val Size = 64
  
  def square(n: Int): Option[BigInt] = 
    if Range.inclusive(1, Size).contains(n)
      then Some(BigInt(1) << (n - 1))
      else None

  def total: BigInt = (BigInt(1) << Size) - 1
}