class Isogram
  @isIsogram: (phrase) ->
    seen = new Set()
    for c in phrase.toUpperCase().replace(/\P{Letter}/ug, '')  # remove non-letters
      return false if seen.has(c)
      seen.add(c)
    return true

module.exports = Isogram
