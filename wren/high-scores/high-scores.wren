class HighScores {
  construct new(scores) {
    _scores = scores
  }

  scores {
    return _scores
  }

  latest {
    return scores[-1]
  }

  /* Return the scores sorted in _descending_ numerical order.
     To prevent sorting the scores in situ, copy the list.
  */
  sorted {
    return scores[0..-1].sort {|a, b| b < a}
  }

  personalBest {
    return sorted[0]
  }

  personalTopThree {
    var n = scores.count.min(3)  // there may be < 3 scores
    return sorted[0..n-1]
  }
}
