import translate from require 'pig_latin'

describe 'pig-latin', ->
  describe 'ay is added to words that start with vowels', ->
    it 'word beginning with a', ->
      assert.are.equal 'appleay', translate 'apple'

    it 'word beginning with e', ->
      assert.are.equal 'earay', translate 'ear'

    it 'word beginning with i', ->
      assert.are.equal 'iglooay', translate 'igloo'

    it 'word beginning with o', ->
      assert.are.equal 'objectay', translate 'object'

    it 'word beginning with u', ->
      assert.are.equal 'underay', translate 'under'

    it 'word beginning with a vowel and followed by a qu', ->
      assert.are.equal 'equalay', translate 'equal'

  describe 'first letter and ay are moved to the end of words that start with consonants', ->
    it 'word beginning with p', ->
      assert.are.equal 'igpay', translate 'pig'

    it 'word beginning with k', ->
      assert.are.equal 'oalakay', translate 'koala'

    it 'word beginning with x', ->
      assert.are.equal 'enonxay', translate 'xenon'

    it 'word beginning with q without a following u', ->
      assert.are.equal 'atqay', translate 'qat'

    it 'word beginning with consonant and vowel containing qu', ->
      assert.are.equal 'iquidlay', translate 'liquid'

  describe 'some letter clusters are treated like a single consonant', ->
    it 'word beginning with ch', ->
      assert.are.equal 'airchay', translate 'chair'

    it 'word beginning with qu', ->
      assert.are.equal 'eenquay', translate 'queen'

    it 'word beginning with qu and a preceding consonant', ->
      assert.are.equal 'aresquay', translate 'square'

    it 'word beginning with th', ->
      assert.are.equal 'erapythay', translate 'therapy'

    it 'word beginning with thr', ->
      assert.are.equal 'ushthray', translate 'thrush'

    it 'word beginning with sch', ->
      assert.are.equal 'oolschay', translate 'school'

  describe 'some letter clusters are treated like a single vowel', ->
    it 'word beginning with yt', ->
      assert.are.equal 'yttriaay', translate 'yttria'

    it 'word beginning with xr', ->
      assert.are.equal 'xrayay', translate 'xray'

  describe 'position of y in a word determines if pending is a consonant or a vowel', ->
    it 'y is treated like a consonant at the beginning of a word', ->
      assert.are.equal 'ellowyay', translate 'yellow'

    it 'y is treated like a vowel at the end of a consonant cluster', ->
      assert.are.equal 'ythmrhay', translate 'rhythm'

    it 'y as second letter in two letter word', ->
      assert.are.equal 'ymay', translate 'my'

  describe 'phrases are translated', ->
    it 'a whole phrase', ->
      assert.are.equal 'ickquay astfay unray', translate 'quick fast run'
