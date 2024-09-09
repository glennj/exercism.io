class Etl
  @transform: (legacy) ->
    result = {}
    for value, letters of legacy
      val = Number.parseInt value
      for letter in letters
        result[letter.toLowerCase()] = val
    result

module.exports = Etl
