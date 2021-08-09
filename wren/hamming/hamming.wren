
class Hamming {
  static validate(len1, len2) {
    if (len1 != len2) {
      if (len1 == 0) {Fiber.abort("left strand must not be empty")}
      if (len2 == 0) {Fiber.abort("right strand must not be empty")}
      Fiber.abort("left and right strands must be of equal length")
    }
  }

  static compute(first, second) {
    validate(first.count, second.count)
    if (first.isEmpty) {
      return 0
    }
    return (0..first.count - 1).reduce(0) {|diff, i|
      return diff + (first[i] == second[i] ? 0 : 1)
    }
  }
}
