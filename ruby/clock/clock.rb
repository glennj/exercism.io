# a clock that knows the hour and minute of the day
class Clock
  private

  attr_reader :hour, :minute

  public

  def initialize(**args)
    h, @minute = (args[:minute] || 0).divmod 60
    @hour = ((args[:hour] || 0) + h) % 24
  end

  def to_s
    format '%02d:%02d', hour, minute
  end

  def to_minutes
    60 * hour + minute
  end

  def +(other)
    self.class.new(minute: to_minutes + other.to_minutes)
  end

  def -(other)
    self.class.new(minute: to_minutes - other.to_minutes)
  end

  def ==(other)
    to_minutes == other.to_minutes
  end
end
