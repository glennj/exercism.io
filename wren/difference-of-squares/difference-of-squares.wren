class Squares {
  static squareOfSum(n) { Seq.sum(1..n).pow(2) }
  static sumOfSquares(n) { Seq.sum((1..n).map {|i| i * i}) }
  static differenceOfSquares(n) { (sumOfSquares(n) - squareOfSum(n)).abs }
}

class Seq {
  static sum(seq) { seq.reduce {|sum, i| sum + i} }
}
