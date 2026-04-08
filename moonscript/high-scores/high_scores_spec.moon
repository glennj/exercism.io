HighScores = require 'high_scores'

describe 'high-scores', ->
  it 'List of scores', ->
    scores = HighScores {30, 50, 20, 70}
    result = scores\scores!
    expected = {30, 50, 20, 70}
    assert.are.same expected, result

  it 'Latest score', ->
    scores = HighScores {100, 0, 90, 30}
    result = scores\latest!
    expected = 30
    assert.are.same expected, result

  it 'Personal best', ->
    scores = HighScores {40, 100, 70}
    result = scores\personalBest!
    expected = 100
    assert.are.same expected, result

  describe 'Top 3 scores', ->
    it 'Personal top three from a list of scores', ->
      scores = HighScores {10, 30, 90, 30, 100, 20, 10, 0, 30, 40, 40, 70, 70}
      result = scores\personalTopThree!
      expected = {100, 90, 70}
      assert.are.same expected, result

    it 'Personal top highest to lowest', ->
      scores = HighScores {20, 10, 30}
      result = scores\personalTopThree!
      expected = {30, 20, 10}
      assert.are.same expected, result

    it 'Personal top when there is a tie', ->
      scores = HighScores {40, 20, 40, 30}
      result = scores\personalTopThree!
      expected = {40, 40, 30}
      assert.are.same expected, result

    it 'Personal top when there are less than 3', ->
      scores = HighScores {30, 70}
      result = scores\personalTopThree!
      expected = {70, 30}
      assert.are.same expected, result

    it 'Personal top when there is only one', ->
      scores = HighScores {40}
      result = scores\personalTopThree!
      expected = {40}
      assert.are.same expected, result

    it 'Latest score after personal top scores', ->
      scores = HighScores {70, 50, 20, 30}
      _ = scores\personalTopThree!
      result = scores\latest!
      expected = 30
      assert.are.same expected, result

    it 'Scores after personal top scores', ->
      scores = HighScores {30, 50, 20, 70}
      _ = scores\personalTopThree!
      result = scores\scores!
      expected = {30, 50, 20, 70}
      assert.are.same expected, result

    it 'Latest score after personal best', ->
      scores = HighScores {20, 70, 15, 25, 30}
      _ = scores\personalBest!
      result = scores\latest!
      expected = 30
      assert.are.same expected, result

    it 'Scores after personal best', ->
      scores = HighScores {20, 70, 15, 25, 30}
      _ = scores\personalBest!
      result = scores\scores!
      expected = {20, 70, 15, 25, 30}
      assert.are.same expected, result
