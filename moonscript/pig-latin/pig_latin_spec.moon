import translate from require 'pig_latin'

describe 'pig-latin:', ->
  describe 'ay is added to words that start with vowels:', ->
    it 'word beginning with a', ->
      result = translate 'apple'
      assert.are.equal 'appleay', result

    it 'word beginning with e', ->
      result = translate 'ear'
      assert.are.equal 'earay', result

    it 'word beginning with i', ->
      result = translate 'igloo'
      assert.are.equal 'iglooay', result

    it 'word beginning with o', ->
      result = translate 'object'
      assert.are.equal 'objectay', result

    it 'word beginning with u', ->
      result = translate 'under'
      assert.are.equal 'underay', result

    it 'word beginning with a vowel and followed by a qu', ->
      result = translate 'equal'
      assert.are.equal 'equalay', result

  describe 'first letter and ay are moved to the end of words that start with consonants:', ->
    it 'word beginning with p', ->
      result = translate 'pig'
      assert.are.equal 'igpay', result

    it 'word beginning with k', ->
      result = translate 'koala'
      assert.are.equal 'oalakay', result

    it 'word beginning with x', ->
      result = translate 'xenon'
      assert.are.equal 'enonxay', result

    it 'word beginning with q without a following u', ->
      result = translate 'qat'
      assert.are.equal 'atqay', result

    it 'word beginning with consonant and vowel containing qu', ->
      result = translate 'liquid'
      assert.are.equal 'iquidlay', result

  describe 'some letter clusters are treated like a single consonant:', ->
    it 'word beginning with ch', ->
      result = translate 'chair'
      assert.are.equal 'airchay', result

    it 'word beginning with qu', ->
      result = translate 'queen'
      assert.are.equal 'eenquay', result

    it 'word beginning with qu and a preceding consonant', ->
      result = translate 'square'
      assert.are.equal 'aresquay', result

    it 'word beginning with th', ->
      result = translate 'therapy'
      assert.are.equal 'erapythay', result

    it 'word beginning with thr', ->
      result = translate 'thrush'
      assert.are.equal 'ushthray', result

    it 'word beginning with sch', ->
      result = translate 'school'
      assert.are.equal 'oolschay', result

  describe 'some letter clusters are treated like a single vowel:', ->
    it 'word beginning with yt', ->
      result = translate 'yttria'
      assert.are.equal 'yttriaay', result

    it 'word beginning with xr', ->
      result = translate 'xray'
      assert.are.equal 'xrayay', result

  describe 'position of y in a word determines if it is a consonant or a vowel:', ->
    it 'y is treated like a consonant at the beginning of a word', ->
      result = translate 'yellow'
      assert.are.equal 'ellowyay', result

    it 'y is treated like a vowel at the end of a consonant cluster', ->
      result = translate 'rhythm'
      assert.are.equal 'ythmrhay', result

    it 'y as second letter in two letter word', ->
      result = translate 'my'
      assert.are.equal 'ymay', result

  describe 'phrases are translated:', ->
    it 'a whole phrase', ->
      result = translate 'quick fast run'
      assert.are.equal 'ickquay astfay unray', result

