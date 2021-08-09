// Re-use code from prior exercise
import "./resistor-color" for Resistor

class ResistorDuo {
  static decodedValue(colors) {
    return colors
      .take(2)
      .reduce(0) {|val, c| 10 * val + Resistor.colorCode(c) }
  }
}

