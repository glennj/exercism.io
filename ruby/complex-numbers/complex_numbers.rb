class ComplexNumber
  attr_reader :real, :imaginary

  def initialize(real, imaginary = 0)
    @real = real
    @imaginary = imaginary
  end

  def ==(other)
    real == other.real && imaginary == other.imaginary
  end

  def +(other)
    self.class.new(real + other.real, imaginary + other.imaginary)
  end

  def -(other)
    self.class.new(real - other.real, imaginary - other.imaginary)
  end

  def *(other)
    r = real * other.real - imaginary * other.imaginary
    i = imaginary * other.real + real * other.imaginary
    self.class.new(r, i)
  end

  def /(other)
    denom = (other.real**2 + other.imaginary**2).to_f
    r = (real * other.real + imaginary * other.imaginary) / denom
    i = (imaginary * other.real - real * other.imaginary) / denom
    self.class.new(r, i)
  end

  def abs
    @abs ||= Math.hypot(real, imaginary)
  end

  def conjugate
    @conjugate ||= self.class.new(real, -imaginary)
  end

  # thanks IEEE floating point numbers, have to round
  def exp
    @exp ||= self.class.new(Math::E**real, 0) \
           * self.class.new(Math.cos(imaginary), Math.sin(imaginary))
  end
end
