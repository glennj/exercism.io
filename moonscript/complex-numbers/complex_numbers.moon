round = (n) ->
  mult = 10^15
  math.floor(0.5 + n * mult) / mult

-- ------------------------------------------------------------
class ComplexNumber
  new: (real, imaginary) =>
    @r = real
    @i = imaginary

  real: => @r
  imaginary: => @i

  conjugate: => @@ @r, -@i

  abs: => math.sqrt(@r*@r + @i*@i)

  exp: =>
    ea = math.exp(1) ^ @r
    @@ round(ea * math.cos @i), round(ea * math.sin @i)

  __eq: (other) => @r == other.r and @i == other.i

  __add: (other) => @@ @r + other.r, @i + other.i
  __sub: (other) => @@ @r - other.r, @i - other.i
  __mul: (other) =>
    a, b, c, d = @r, @i, other.r, other.i
    @@ a*c - b*d, b*c + a*d
  __div: (other) =>
    a, b, c, d = @r, @i, other.r, other.i
    denom = c*c + d*d
    @@ (a*c + b*d) / denom, (b*c - a*d) / denom

  -- a class method, `self` is the class object
  @tocomplex: (thing) ->
    t = type thing
    if t == 'table' and thing.__class == self
      thing
    elseif t == 'number'
      self thing, 0
    else
      error "Don't know how to convert this thing to a ComplexNumber"

-- ------------------------------------------------------------
-- For arithmetic operations between real and complex numbers

add = (a, b) -> ComplexNumber.tocomplex(a) + ComplexNumber.tocomplex(b)
sub = (a, b) -> ComplexNumber.tocomplex(a) - ComplexNumber.tocomplex(b)
mul = (a, b) -> ComplexNumber.tocomplex(a) * ComplexNumber.tocomplex(b)
div = (a, b) -> ComplexNumber.tocomplex(a) / ComplexNumber.tocomplex(b)

-- ------------------------------------------------------------
{ :ComplexNumber, :add, :sub, :mul, :div }
