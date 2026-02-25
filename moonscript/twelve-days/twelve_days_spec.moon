TwelveDays = require 'twelve_days'

describe 'twelve-days', ->
  describe 'verse', ->
    it 'first day a partridge in a pear tree', ->
      result = TwelveDays.recite 1, 1
      expected = {'On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'second day two turtle doves', ->
      result = TwelveDays.recite 2, 2
      expected = {'On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'third day three french hens', ->
      result = TwelveDays.recite 3, 3
      expected = {'On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'fourth day four calling birds', ->
      result = TwelveDays.recite 4, 4
      expected = {'On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'fifth day five gold rings', ->
      result = TwelveDays.recite 5, 5
      expected = {'On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'sixth day six geese-a-laying', ->
      result = TwelveDays.recite 6, 6
      expected = {'On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'seventh day seven swans-a-swimming', ->
      result = TwelveDays.recite 7, 7
      expected = {'On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'eighth day eight maids-a-milking', ->
      result = TwelveDays.recite 8, 8
      expected = {'On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'ninth day nine ladies dancing', ->
      result = TwelveDays.recite 9, 9
      expected = {'On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'tenth day ten lords-a-leaping', ->
      result = TwelveDays.recite 10, 10
      expected = {'On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'eleventh day eleven pipers piping', ->
      result = TwelveDays.recite 11, 11
      expected = {'On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

    it 'twelfth day twelve drummers drumming', ->
      result = TwelveDays.recite 12, 12
      expected = {'On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.'}
      assert.are.same expected, result

  describe 'lyrics', ->
    it 'recites first three verses of the song', ->
      result = TwelveDays.recite 1, 3
      expected = {
        'On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree.',
        'On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
      }
      assert.are.same expected, result

    it 'recites three verses from the middle of the song', ->
      result = TwelveDays.recite 4, 6
      expected = {
        'On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
      }
      assert.are.same expected, result

    it 'recites the whole song', ->
      result = TwelveDays.recite 1, 12
      expected = {
        'On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree.',
        'On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
        'On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.',
      }
      assert.are.same expected, result
