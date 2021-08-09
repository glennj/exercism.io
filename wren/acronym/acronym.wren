import "./byte" for Byte

class Acronym {
  /*
   * A little state machine:
   * - state == "A", we're looking for the next acronym letter;
   * - state == "N", we're looking for the end of the current word.
   */
  static parse(phrase) {
    var state = "A"

    return phrase.bytes
      .map {|byte| Byte.new(byte)}
      .where {|b|
        if (state == "A" && b.isAlpha) {
          state = "N"
          return true
        } else if (state == "N" && !(b.isAlpha || b.isApostrophe)) {
          state = "A"
        }
      }
      .map {|b| b.upcase.toString}
      .join()
  }
}
