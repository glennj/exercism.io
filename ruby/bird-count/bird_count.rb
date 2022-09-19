class BirdCount
  def self.last_week
    [0, 2, 5, 3, 7, 8, 4]
  end

  def initialize(birds_per_day)
    @daily = birds_per_day
  end

  def yesterday
    @daily[-2]
  end

  def total
    @daily.sum
  end

  def busy_days
    @daily.count {|birds| birds >= 5}
  end

  def day_without_birds?
    @daily.any? {|birds| birds == 0}
  end
end
