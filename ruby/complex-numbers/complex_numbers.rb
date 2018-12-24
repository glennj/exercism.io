class ComplexNumber
  attr_reader :real, :imaginary

  def initialize(real, imaginary)
    @real = real
    @imaginary = imaginary
  end

  def ==(other)
    @real == other.real && @imaginary == other.imaginary
  end

  def +(other)
    ComplexNumber.new(@real + other.real, @imaginary + other.imaginary)
  end

  def -(other)
    ComplexNumber.new(@real - other.real, @imaginary - other.imaginary)
  end

  def *(other)
    r = @real * other.real - @imaginary * other.imaginary
    i = @imaginary * other.real + @real * other.imaginary
    ComplexNumber.new(r, i)
  end

  def /(other)
    denom = (other.real**2 + other.imaginary**2).to_f
    r = (@real * other.real + @imaginary * other.imaginary) / denom
    i = (@imaginary * other.real - @real * other.imaginary) / denom
    ComplexNumber.new(r, i)
  end

  def abs
    return @abs if @abs
    @abs = Math.sqrt(@real**2 + @imaginary**2)
  end

  def conjugate
    return @conj if @conj
    @conj = ComplexNumber.new(@real, -@imaginary)
  end

  def exp
    return @exp if @exp
    @exp = ComplexNumber.new(Math::E**@real, 0) \
           * ComplexNumber.new(Math.cos(@imaginary), Math.sin(@imaginary))
  end
end
