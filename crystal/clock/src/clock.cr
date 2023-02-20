class Clock
  getter minutes : Int32

  def initialize(hour = 0, minute = 0)
    @minutes = normalize(hour * 60 + minute)
  end

  def to_s
    "%02d:%02d" % [minutes // 60, minutes % 60]
  end

  def +(other)
    @minutes = normalize(minutes + other.minutes)
    self
  end

  def -(other)
    @minutes = normalize(minutes - other.minutes)
    self
  end

  def ==(other)
    minutes == other.minutes
  end

  private def normalize(mins)
    mins % (24 * 60)
  end

end
