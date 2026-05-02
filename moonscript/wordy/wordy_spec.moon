Wordy = require 'wordy'

describe 'wordy', ->
  it 'just a number', ->
    result = Wordy.answer 'What is 5?'
    assert.are.equal 5, result

  it 'just a zero', ->
    result = Wordy.answer 'What is 0?'
    assert.are.equal 0, result

  it 'just a negative number', ->
    result = Wordy.answer 'What is -123?'
    assert.are.equal -123, result

  it 'addition', ->
    result = Wordy.answer 'What is 1 plus 1?'
    assert.are.equal 2, result

  it 'addition with a left hand zero', ->
    result = Wordy.answer 'What is 0 plus 2?'
    assert.are.equal 2, result

  it 'addition with a right hand zero', ->
    result = Wordy.answer 'What is 3 plus 0?'
    assert.are.equal 3, result

  it 'more addition', ->
    result = Wordy.answer 'What is 53 plus 2?'
    assert.are.equal 55, result

  it 'addition with negative numbers', ->
    result = Wordy.answer 'What is -1 plus -10?'
    assert.are.equal -11, result

  it 'large addition', ->
    result = Wordy.answer 'What is 123 plus 45678?'
    assert.are.equal 45801, result

  it 'subtraction', ->
    result = Wordy.answer 'What is 4 minus -12?'
    assert.are.equal 16, result

  it 'multiplication', ->
    result = Wordy.answer 'What is -3 multiplied by 25?'
    assert.are.equal -75, result

  it 'division', ->
    result = Wordy.answer 'What is 33 divided by -3?'
    assert.are.equal -11, result

  it 'multiple additions', ->
    result = Wordy.answer 'What is 1 plus 1 plus 1?'
    assert.are.equal 3, result

  it 'addition and subtraction', ->
    result = Wordy.answer 'What is 1 plus 5 minus -2?'
    assert.are.equal 8, result

  it 'multiple subtraction', ->
    result = Wordy.answer 'What is 20 minus 4 minus 13?'
    assert.are.equal 3, result

  it 'subtraction then addition', ->
    result = Wordy.answer 'What is 17 minus 6 plus 3?'
    assert.are.equal 14, result

  it 'multiple multiplication', ->
    result = Wordy.answer 'What is 2 multiplied by -2 multiplied by 3?'
    assert.are.equal -12, result

  it 'addition and multiplication', ->
    result = Wordy.answer 'What is -3 plus 7 multiplied by -2?'
    assert.are.equal -8, result

  it 'multiple division', ->
    result = Wordy.answer 'What is -12 divided by 2 divided by -3?'
    assert.are.equal 2, result

  it 'unknown operation', ->
    fn = -> Wordy.answer 'What is 52 cubed?'
    assert.has.error fn, 'unknown operation'

  it 'Non math question', ->
    fn = -> Wordy.answer 'Who is the President of the United States?'
    assert.has.error fn, 'unknown operation'

  it 'reject problem missing an operand', ->
    fn = -> Wordy.answer 'What is 1 plus?'
    assert.has.error fn, 'syntax error'

  it 'reject problem with no operands or operators', ->
    fn = -> Wordy.answer 'What is?'
    assert.has.error fn, 'syntax error'

  it 'reject two operations in a row', ->
    fn = -> Wordy.answer 'What is 1 plus plus 2?'
    assert.has.error fn, 'syntax error'

  it 'reject two numbers in a row', ->
    fn = -> Wordy.answer 'What is 1 plus 2 1?'
    assert.has.error fn, 'syntax error'

  it 'reject postfix notation', ->
    fn = -> Wordy.answer 'What is 1 2 plus?'
    assert.has.error fn, 'syntax error'

  it 'reject prefix notation', ->
    fn = -> Wordy.answer 'What is plus 1 2?'
    assert.has.error fn, 'syntax error'
