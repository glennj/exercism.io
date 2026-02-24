Bob = require 'bob'

describe 'bob', ->
  it 'stating something', ->
    result = Bob.hey "Tom-ay-to, tom-aaaah-to."
    assert.is.equal 'Whatever.', result

  it 'shouting', ->
    result = Bob.hey "WATCH OUT!"
    assert.is.equal 'Whoa, chill out!', result

  it 'shouting gibberish', ->
    result = Bob.hey "FCECDFCAAB"
    assert.is.equal 'Whoa, chill out!', result

  it 'asking a question', ->
    result = Bob.hey "Does this cryogenic chamber make me look fat?"
    assert.is.equal 'Sure.', result

  it 'asking a numeric question', ->
    result = Bob.hey "You are, what, like 15?"
    assert.is.equal 'Sure.', result

  it 'asking gibberish', ->
    result = Bob.hey "fffbbcbeab?"
    assert.is.equal 'Sure.', result

  it 'talking forcefully', ->
    result = Bob.hey "Hi there!"
    assert.is.equal 'Whatever.', result

  it 'using acronyms in regular speech', ->
    result = Bob.hey "It's OK if you don't want to go work for NASA."
    assert.is.equal 'Whatever.', result

  it 'forceful question', ->
    result = Bob.hey "WHAT'S GOING ON?"
    assert.is.equal "Calm down, I know what I'm doing!", result

  it 'shouting numbers', ->
    result = Bob.hey "1, 2, 3 GO!"
    assert.is.equal 'Whoa, chill out!', result

  it 'no letters', ->
    result = Bob.hey "1, 2, 3"
    assert.is.equal 'Whatever.', result

  it 'question with no letters', ->
    result = Bob.hey "4?"
    assert.is.equal 'Sure.', result

  it 'shouting with special characters', ->
    result = Bob.hey "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"
    assert.is.equal 'Whoa, chill out!', result

  it 'shouting with no exclamation mark', ->
    result = Bob.hey "I HATE THE DENTIST"
    assert.is.equal 'Whoa, chill out!', result

  it 'statement containing question mark', ->
    result = Bob.hey "Ending with ? means a question."
    assert.is.equal 'Whatever.', result

  it 'non-letters with question', ->
    result = Bob.hey ":) ?"
    assert.is.equal 'Sure.', result

  it 'prattling on', ->
    result = Bob.hey "Wait! Hang on. Are you going to be OK?"
    assert.is.equal 'Sure.', result

  it 'silence', ->
    result = Bob.hey ""
    assert.is.equal 'Fine. Be that way!', result

  it 'prolonged silence', ->
    result = Bob.hey "          "
    assert.is.equal 'Fine. Be that way!', result

  it 'alternate silence', ->
    result = Bob.hey "\t\t\t\t\t\t\t\t\t\t"
    assert.is.equal 'Fine. Be that way!', result

  it 'starting with whitespace', ->
    result = Bob.hey "         hmmmmmmm..."
    assert.is.equal 'Whatever.', result

  it 'ending with whitespace', ->
    result = Bob.hey "Okay if like my  spacebar  quite a bit?   "
    assert.is.equal 'Sure.', result

  it 'other whitespace', ->
    result = Bob.hey "\n\r \t"
    assert.is.equal 'Fine. Be that way!', result

  it 'non-question ending with whitespace', ->
    result = Bob.hey "This is a statement ending with whitespace      "
    assert.is.equal 'Whatever.', result

  it 'multiple line question', ->
    result = Bob.hey "\nDoes this cryogenic chamber make\n me look fat?"
    assert.is.equal 'Sure.', result
