class HighScores {
  construct new(scores) {
    _scores = scores
  }
  scores { _scores }
  latest { scores[-1] }

  // Return the scores sorted in _descending_ numerical order.
  // To prevent sorting the scores in situ, copy the list.
  sorted { scores[0..-1].sort {|a, b| b < a} }
  personalBest { sorted[0] }

  personalTopThree {
    var n = scores.count.min(3)  // there may be < 3 scores
    return sorted[0...n]
  }
}
