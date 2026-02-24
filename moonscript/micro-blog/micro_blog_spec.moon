MicroBlog = require 'micro_blog'

describe 'micro-blog', ->
  it 'English language short', ->
    result = MicroBlog.truncate 'Hi'
    assert.are.equal 'Hi', result

  it 'English language long', ->
    result = MicroBlog.truncate 'Hello there'
    assert.are.equal 'Hello', result

  it 'German language short (broth)', ->
    result = MicroBlog.truncate 'brühe'
    assert.are.equal 'brühe', result

  it 'German language long (bear carpet → beards)', ->
    result = MicroBlog.truncate 'Bärteppich'
    assert.are.equal 'Bärte', result

  it 'Bulgarian language short (good)', ->
    result = MicroBlog.truncate 'Добър'
    assert.are.equal 'Добър', result

  it 'Greek language short (health)', ->
    result = MicroBlog.truncate 'υγειά'
    assert.are.equal 'υγειά', result

  it 'Maths short', ->
    result = MicroBlog.truncate 'a=πr²'
    assert.are.equal 'a=πr²', result

  it 'Maths long', ->
    result = MicroBlog.truncate '∅⊊ℕ⊊ℤ⊊ℚ⊊ℝ⊊ℂ'
    assert.are.equal '∅⊊ℕ⊊ℤ', result

  it 'English and emoji short', ->
    result = MicroBlog.truncate 'Fly 🛫'
    assert.are.equal 'Fly 🛫', result

  it 'Emoji short', ->
    result = MicroBlog.truncate '💇'
    assert.are.equal '💇', result

  it 'Emoji long', ->
    result = MicroBlog.truncate '❄🌡🤧🤒🏥🕰😀'
    assert.are.equal '❄🌡🤧🤒🏥', result

  it 'Royal Flush?', ->
    result = MicroBlog.truncate '🃎🂸🃅🃋🃍🃁🃊'
    assert.are.equal '🃎🂸🃅🃋🃍', result
