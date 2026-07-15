Bob = require 'bob'

describe 'bob:', ->
  it 'asking a question', ->
    result = Bob.hey "Does this cryogenic chamber make me look fat?"
    expected = 'Sure.'
    assert.are.equal expected, result

  it 'shouting', ->
    result = Bob.hey "WATCH OUT!"
    expected = 'Whoa, chill out!'
    assert.are.equal expected, result

  it 'forceful question', ->
    result = Bob.hey "WHAT'S GOING ON?"
    expected = "Calm down, I know what I'm doing!"
    assert.are.equal expected, result

  it 'silence', ->
    result = Bob.hey ""
    expected = 'Fine. Be that way!'
    assert.are.equal expected, result

  it 'stating something', ->
    result = Bob.hey "Tom-ay-to, tom-aaaah-to."
    expected = 'Whatever.'
    assert.are.equal expected, result

  it 'asking a numeric question', ->
    result = Bob.hey "You are, what, like 15?"
    expected = 'Sure.'
    assert.are.equal expected, result

  it 'asking gibberish', ->
    result = Bob.hey "fffbbcbeab?"
    expected = 'Sure.'
    assert.are.equal expected, result

  it 'question with no letters', ->
    result = Bob.hey "4?"
    expected = 'Sure.'
    assert.are.equal expected, result

  it 'non-letters with question', ->
    result = Bob.hey ":) ?"
    expected = 'Sure.'
    assert.are.equal expected, result

  it 'prattling on', ->
    result = Bob.hey "Wait! Hang on. Are you going to be OK?"
    expected = 'Sure.'
    assert.are.equal expected, result

  it 'ending with whitespace', ->
    result = Bob.hey "Okay if like my  spacebar  quite a bit?   "
    expected = 'Sure.'
    assert.are.equal expected, result

  it 'multiple line question', ->
    result = Bob.hey "\nDoes this cryogenic chamber make\n me look fat?"
    expected = 'Sure.'
    assert.are.equal expected, result

  it 'shouting gibberish', ->
    result = Bob.hey "FCECDFCAAB"
    expected = 'Whoa, chill out!'
    assert.are.equal expected, result

  it 'shouting a statement containing a question mark', ->
    result = Bob.hey "DO LIONS EAT PEOPLE? AHHHHH."
    expected = 'Whoa, chill out!'
    assert.are.equal expected, result

  it 'shouting numbers', ->
    result = Bob.hey "1, 2, 3 GO!"
    expected = 'Whoa, chill out!'
    assert.are.equal expected, result

  it 'shouting with special characters', ->
    result = Bob.hey "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"
    expected = 'Whoa, chill out!'
    assert.are.equal expected, result

  it 'shouting with no exclamation mark', ->
    result = Bob.hey "I HATE THE DENTIST"
    expected = 'Whoa, chill out!'
    assert.are.equal expected, result

  it 'prolonged silence', ->
    result = Bob.hey "          "
    expected = 'Fine. Be that way!'
    assert.are.equal expected, result

  it 'alternate silence', ->
    result = Bob.hey "\t\t\t\t\t\t\t\t\t\t"
    expected = 'Fine. Be that way!'
    assert.are.equal expected, result

  it 'other whitespace', ->
    result = Bob.hey "\n\r \t"
    expected = 'Fine. Be that way!'
    assert.are.equal expected, result

  it 'talking forcefully', ->
    result = Bob.hey "Hi there!"
    expected = 'Whatever.'
    assert.are.equal expected, result

  it 'using acronyms in regular speech', ->
    result = Bob.hey "It's OK if you don't want to go work for NASA."
    expected = 'Whatever.'
    assert.are.equal expected, result

  it 'no letters', ->
    result = Bob.hey "1, 2, 3"
    expected = 'Whatever.'
    assert.are.equal expected, result

  it 'statement containing question mark', ->
    result = Bob.hey "Ending with ? means a question."
    expected = 'Whatever.'
    assert.are.equal expected, result

  it 'starting with whitespace', ->
    result = Bob.hey "         hmmmmmmm..."
    expected = 'Whatever.'
    assert.are.equal expected, result

  it 'non-question ending with whitespace', ->
    result = Bob.hey "This is a statement ending with whitespace      "
    expected = 'Whatever.'
    assert.are.equal expected, result

