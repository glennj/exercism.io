class Grains {
  static square(num) { 
    if (!(1..64).contains(num)) {
      Fiber.abort("square must be between 1 and 64")
    }
    return 2.pow(num - 1) 
  }

  static total { 2.pow(64) - 1 }
  /* alternately:
  static total { (1..64).map {|sq| square(sq)}.reduce {|sum, gr| sum + gr} }
  */
}
