class Hamming {
  static compute(first, second) {
    if (first.count != second.count) {
      Fiber.abort("strands must be of equal length")
    }
    return (0...first.count).where {|i| first[i] != second[i]}.count
  }
}
