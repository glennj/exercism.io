class AllYourBase {
  static assert(func, msg) {
    if (!func.call()) {
      Fiber.abort(msg)
    }
  }

  static rebase(inputBase, digits, outputBase) {
    assert(Fn.new {inputBase >= 2}, "input base must be >= 2")
    assert(Fn.new {outputBase >= 2}, "output base must be >= 2")
    assert(
        Fn.new {digits.all {|d| (0...inputBase).contains(d)}},
        "all digits must satisfy 0 <= d < input base"
    )

    var decimal = digits.reduce(0) {|acc, digit| acc * inputBase + digit}

    var result = []
    while (decimal >= outputBase) {
      result.insert(0, decimal % outputBase)
      decimal = (decimal / outputBase).floor
    }
    result.insert(0, decimal)
    return result
  }
}
