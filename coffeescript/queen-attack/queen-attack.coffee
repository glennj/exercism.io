class QueenAttack
  constructor: ({@row, @column}) ->
    throw new Error 'row not positive'    if @row    < 0
    throw new Error 'column not positive' if @column < 0
    throw new Error 'row not on board'    if @row    > 7
    throw new Error 'column not on board' if @column > 7

  canAttack: (other) ->
    @row == other.row or 
    @column == other.column or
    Math.abs(@row - other.row) == Math.abs(@column - other.column)

module.exports = QueenAttack
