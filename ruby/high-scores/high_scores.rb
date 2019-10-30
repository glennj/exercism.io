class HighScores
  def initialize(scores)
    @scores = scores
  end
  attr_reader :scores

  def latest
    @scores.last
  end

  def personal_best
    @scores.max
  end

  def personal_top_three
    @scores.sort.reverse.slice(0, 3)
  end

  def latest_is_personal_best?
    latest == personal_best
  end
end
