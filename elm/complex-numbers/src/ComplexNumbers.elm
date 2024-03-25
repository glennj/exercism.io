module ComplexNumbers exposing
    ( Complex
    , abs
    , add
    , conjugate
    , div
    , exp
    , fromPair
    , fromReal
    , imaginary
    , mul
    , real
    , sub
    )


type alias Complex =
    { real : Float, imag : Float }


fromPair : ( Float, Float ) -> Complex
fromPair ( realPart, imaginaryPart ) =
    Complex realPart imaginaryPart


fromReal : Float -> Complex
fromReal realPart =
    Complex realPart 0


real : Complex -> Float
real z =
    z.real


imaginary : Complex -> Float
imaginary z =
    z.imag


conjugate : Complex -> Complex
conjugate z =
    Complex z.real -z.imag


abs : Complex -> Float
abs z =
    sqrt (z.real ^ 2 + z.imag ^ 2)


add : Complex -> Complex -> Complex
add z1 z2 =
    Complex (z1.real + z2.real) (z1.imag + z2.imag)


sub : Complex -> Complex -> Complex
sub z1 z2 =
    Complex (z1.real - z2.real) (z1.imag - z2.imag)


mul : Complex -> Complex -> Complex
mul z1 z2 =
    let
        r = (z1.real * z2.real) - (z1.imag * z2.imag)
        i = (z1.imag * z2.real) + (z1.real * z2.imag)
    in
    Complex r i


div : Complex -> Complex -> Complex
div z1 z2 =
    let
        d = z2.real ^ 2 + z2.imag ^ 2
        r = ((z1.real * z2.real) + (z1.imag * z2.imag)) / d
        i = ((z1.imag * z2.real) - (z1.real * z2.imag)) / d
    in
    Complex r i


exp : Complex -> Complex
exp z =
    mul (fromReal (e ^ z.real))
        (fromPair ( cos z.imag, sin z.imag ))
