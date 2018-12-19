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

  def personal_top
    @scores.sort.reverse.slice(0, 3)
  end

  def report
    delta = if latest < personal_best
      "#{personal_best - latest} short of "
    else
      ''
    end
    "Your latest score was #{latest}. That's #{delta}your personal best!"
  end
end
