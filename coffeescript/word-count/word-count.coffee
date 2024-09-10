class WordCount
  constructor: (@input) ->

  count: ->
    @input.toLowerCase().match(/\w+(?:'\w+)?/gi).reduce(incr, {})

incr = (obj, key) ->
  obj[key] = (obj[key] ? 0) + 1
  obj

module.exports = WordCount
