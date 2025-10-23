class Assert {
  static assert(condition, msg) {
    if (!condition) Fiber.abort(msg)
  }

  static refute(condition, msg) { assert(!condition, msg) }
}
