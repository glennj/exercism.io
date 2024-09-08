ResistorColor = require './resistor-color'

# just for fun, monkeypatch Array
Array.prototype.take = (n) -> @slice(0, n)

class ResistorColorDuo
  @value: (colors) ->
    colors
      .take(2)
      .map(ResistorColor.colorCode)
      .reduce((val, code) -> val * 10 + code)

module.exports = ResistorColorDuo
