import "./resistor-color" for Resistor
import "./resistor-color-duo" for ResistorDuo

class ResistorColorTrio {
  construct new(colors) {
    _colors = colors[0..2]
  }

  colors { _colors }

  resistorValue {
    var duo = ResistorDuo.decodedValue(colors.take(2)) 
    var exp = Resistor.colorCode(colors[2])
    return duo * 10.pow(exp)
  }

  label {
    var val = resistorValue

    var idx = 0
    while (val >= 1000) {
      idx = idx + 1
      val = val / 1000
    }

    var prefix = ["", "kilo", "mega", "giga"][idx]

    return "Resistor value: %(val) %(prefix)ohms"
  }
}
