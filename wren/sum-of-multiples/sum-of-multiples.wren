class SumOfMultiples {
  static sum(factors, limit) { (1...limit) .
      where {|n| factors.any {|f| n % f == 0}} .
      reduce(0) {|sum, n| sum + n}
  }
}
