object Darts {
  def score(x: Double, y: Double): Int =
    scala.math.hypot(x, y) match
      case z if z <=  1.0 => 10
      case z if z <=  5.0 =>  5
      case z if z <= 10.0 =>  1
      case _              =>  0
}