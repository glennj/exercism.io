class SpaceAge
  def initialize(seconds)
    @earth_age = seconds.to_f / 31_557_600
  end

  RELATIVE_YEARS = {
    mercury: 0.2408467,
    venus:   0.61519726,
    earth:   1.0,
    mars:    1.8808158,
    jupiter: 11.862615,
    saturn:  29.447498,
    uranus:  84.016846,
    neptune: 164.79132
  }.freeze

  RELATIVE_YEARS.each_pair do |p, y|
    define_method(:"on_#{p}") { @earth_age / y }
  end

  # alternately:
  #
  #  def method_missing(method)
  #    m = method.to_s.match(/^on_(\w+)$/)
  #    super if m.nil?
  #    @earth_age / RELATIVE_YEARS[m[1].to_sym]
  #  end
  #
  #  def respond_to_missing?(method)
  #    m = method.match(/^on_(\w+)$/)
  #    super if m.nil?
  #    RELATIVE_YEARS.key? m[1].to_sym
  #  end
end
