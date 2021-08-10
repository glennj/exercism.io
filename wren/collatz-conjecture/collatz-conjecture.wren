class Collatz {

  static steps(n) {
    if (n <= 0) Fiber.abort("Only positive numbers are allowed")
    //return steps_iterative_(n)
    return steps_recursive_(n)
  }

  static next_(n) { (n % 2 == 0) ? (n / 2) : (3 * n + 1) }

  static steps_iterative_(n) {
    var steps = 0
    while (n > 1) {
      steps = steps + 1
      n = next_(n)
    }
    return steps
  }

  /* I don't think wren implements tailcall recursion, but
   * if it did...
   */
  static steps_recursive_(n) { steps_tailcall_(n, 0) }

  static steps_tailcall_(n, steps) {
    if (n == 1) return steps
    return steps_tailcall_(next_(n), steps + 1)
  }
}
