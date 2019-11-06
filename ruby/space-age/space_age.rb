class SpaceAge
  private

  attr_reader :earth_years

  RELATIVE_YEARS = {
    mercury:   0.2408467,
    venus:     0.61519726,
    earth:     1.0,
    mars:      1.8808158,
    jupiter:  11.862615,
    saturn:   29.447498,
    uranus:   84.016846,
    neptune: 164.79132
  }.freeze

  SECONDS_PER_EARTH_YEAR = 31_557_600

  public

  def initialize(seconds)
    @earth_years = seconds.to_f / SECONDS_PER_EARTH_YEAR
  end

  RELATIVE_YEARS.each_pair do |p, r|
    define_method(:"on_#{p}") { earth_years / r }
  end

  # alternately:
  #
  #  def method_missing(method)
  #    m = method.to_s.match(/^on_(\w+)$/)
  #    super if m.nil?
  #    earth_years / RELATIVE_YEARS[m[1].to_sym]
  #  end
  #
  #  def respond_to_missing?(method)
  #    m = method.match(/^on_(\w+)$/)
  #    super if m.nil?
  #    RELATIVE_YEARS.key? m[1].to_sym
  #  end
end
