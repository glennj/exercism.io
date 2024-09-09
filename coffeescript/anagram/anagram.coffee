class Anagram
  toKey = (word) -> [word...].sort().join('')
  
  constructor: (@source) ->
    @srcLower = @source.toLowerCase()
    @srcKey = toKey @srcLower

  match: (targets) -> (w for w in targets when @isAnagram w)

  isAnagram: (word) ->
    lower = word.toLowerCase()
    @srcLower isnt lower and @srcKey is toKey lower

module.exports = Anagram
