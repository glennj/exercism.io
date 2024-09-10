class Wordy
  constructor: (@question) ->

  ERROR:
    syntaxError: 'syntax error'
    unknownOperation: 'unknown operation'

  errSyntax: -> throw new Error @ERROR.syntaxError
  errUnknownOp: -> throw new Error @ERROR.unknownOperation

  answer: ->
    @errUnknownOp() unless @question.startsWith 'What is'
    @errUnknownOp() unless @question.endsWith '?'

    @stack = @question.replace(/^What is\s*(.*)\?$/, '$1').split(/\s+/)

    result = @getNumber()
    while @stack.length > 0
      op = @getOperation()
      number = @getNumber()
      result = @performOperation result, op, number
    result

  getNumber: ->
    n = Number.parseInt @stack.shift()
    @errSyntax() if Number.isNaN n
    n

  getOperation: ->
    switch op = @stack.shift()
      when 'plus', 'minus' then break
      when 'multiplied', 'divided'
        @errSyntax() unless @stack.shift() == 'by'
      else 
        if Number.isNaN Number.parseInt op
          @errUnknownOp()
        else
          @errSyntax()
    op

  performOperation: (a, op, b) ->
    switch op
      when 'plus' then a + b
      when 'minus' then a - b
      when 'multiplied' then a * b
      when 'divided' then a / b     # TODO div by zero


module.exports = Wordy

