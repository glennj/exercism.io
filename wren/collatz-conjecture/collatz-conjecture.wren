
class Collatz {
  static steps(n) {
    if (n <= 0) {
      Fiber.abort("Only positive numbers are allowed")
    }
    if (n == 1) {
      return 0
    }
    return 1 + steps(n % 2 == 0 ? (n / 2) : (3 * n + 1))
  }
}
