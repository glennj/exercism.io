import Base: show, +, -, *, /, ^, ==, ≈, conj, real, imag, exp, abs


struct ComplexNumber{T<:Real} <: Number
    real::T
    imag::T
end

ComplexNumber(x::Real) = ComplexNumber(x, zero(x))
ComplexNumber(x::Real, y::Real) = ComplexNumber(promote(x,y)...)


# sugar
const jm = ComplexNumber(0, 1)


# expose struct members
real(c::ComplexNumber) = c.real
imag(c::ComplexNumber) = c.imag


function show(io::IO, c::ComplexNumber)
    show(io, real(c))
    i = imag(c)
    if i < 0
        i = -i
        print(io, " - ")
    else
        print(io, " + ")
    end
    show(io, i)
    print(io, "jm")
end


# equality
function ==(c::ComplexNumber, d::ComplexNumber)
    real(c) == real(d) && imag(c) == imag(d)
end

# approximate equality
function ≈(c::ComplexNumber, d::ComplexNumber)
    m = [real(c), imag(c), real(d), imag(d)] .|> abs |> maximum
    isapprox(real(c), real(c), atol = eps(m)) &&
    isapprox(imag(c), imag(c), atol = eps(m))
end


# absolute value
abs(c::ComplexNumber) = hypot(real(c), imag(c))


# raise to an integral power
function ^(c::ComplexNumber, n::Integer)
    @assert n > 0
    reduce(*, c for i in 1:n)
end


# addition
function +(c::ComplexNumber, d::ComplexNumber)
    ComplexNumber(real(c) + real(d), imag(c) + imag(d))
end
+(x::Real, c::ComplexNumber) = ComplexNumber(x + real(c), imag(c))


# negation
-(c::ComplexNumber) = ComplexNumber(-real(c), -imag(c))


# subtraction
-(c::ComplexNumber, d::ComplexNumber) = c + -d
-(x::Real, c::ComplexNumber) = ComplexNumber(x - real(c), imag(c))


# multiplication
function *(c::ComplexNumber, d::ComplexNumber)
    ComplexNumber(
        real(c) * real(d) - imag(c) * imag(d),
        imag(c) * real(d) + real(c) * imag(d)
    )
end
*(x::Real, c::ComplexNumber) = ComplexNumber(x * real(c), x * imag(c))


# division
function /(c::ComplexNumber, d::ComplexNumber)
    denom = real(d)^2 + imag(d)^2
    @assert(denom ≠ 0, "Division by zero")
    ComplexNumber(
        (real(c) * real(d) + imag(c) * imag(d)) / denom,
        (imag(c) * real(d) - real(c) * imag(d)) / denom
    )
end


# conjugate
function conj(c::ComplexNumber)
    real(c) + (-imag(c))jm
end


# exponentiation
function exp(c::ComplexNumber)
    r = real(c)
    θ = imag(c)
    exp(r) * (cos(θ) + sin(θ)jm)
end
