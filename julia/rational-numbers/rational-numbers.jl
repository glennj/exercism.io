import Base: show, zero, one, +, -, *, /, ==, !=, <, <=, >, >=, abs, ^, float

struct RationalNumber{T<:Integer} <: Real
    n::T     # numerator
    d::T     # denominator

    function RationalNumber{T}(n::Integer, d::Integer) where {T<:Integer}
        if n == d == zero(T)
            throw(ArgumentError("Invalid rational number: $(n)//$(d)"))
        end
        _n, _d = _normalize(n, d)
        new(_n, _d)
    end
end

RationalNumber(n::T, d::T) where {T<:Integer} = RationalNumber{T}(n,d)
RationalNumber(n::Integer, d::Integer) = RationalNumber(promote(n,d)...)
RationalNumber(n::Integer) = RationalNumber(n,one(n))

function show(io::IO, r::RationalNumber)
    print(io, "$(r.n)//$(r.d)")
end

num(r::RationalNumber)   = r.n
den(r::RationalNumber) = r.d

numerator(r::RationalNumber)   = num(r)
denominator(r::RationalNumber) = den(r)

function _normalize(n::T, d::T) where {T<:Integer}
    g = gcd(n, d)
    m = d < 0 ? -1 : 1
    (m * n ÷ g, m * d ÷ g)
end

zero(::Type{RationalNumber{Int}}) = RationalNumber(0, 1)
one(::Type{RationalNumber{Int}})  = RationalNumber(1, 1)

iszero(r::RationalNumber) = iszero(num(r))
isone(r::RationalNumber)  = isone(num(r)) && isone(den(r))

isfinite(r::RationalNumber) = !iszero(den(r))

# convert to Float
function Core.AbstractFloat(x::RationalNumber)
    float(num(x)) / float(den(x))
end

# addition
function +(a::RationalNumber, b::RationalNumber)
    n = num(a) * den(b) + den(a) * num(b)
    d = den(a) * den(b)
    RationalNumber(n, d)
end
+(a::Integer, b::RationalNumber) = RationalNumber(a) + b
+(a::RationalNumber, b::Integer) = a + RationalNumber(b)

# negation
function -(r::RationalNumber)
    RationalNumber(-num(r), den(r))
end

# subtraction
-(a::RationalNumber, b::RationalNumber) = a + -b
-(a::Integer, b::RationalNumber) = RationalNumber(a) + b
-(a::RationalNumber, b::Integer) = a + RationalNumber(b)

# multiplication
function *(a::RationalNumber, b::RationalNumber)
    RationalNumber(num(a) * num(b), den(a) * den(b))
end
*(a::Integer, b::RationalNumber) = RationalNumber(a) * b
*(a::RationalNumber, b::Integer) = a * RationalNumber(b)

# division
function /(a::RationalNumber, b::RationalNumber)
    RationalNumber(num(a) * den(b), den(a) * num(b))
end
/(a::Integer, b::RationalNumber) = RationalNumber(a) / b
/(a::RationalNumber, b::Integer) = a / RationalNumber(b)

# exponentiation
function ^(r::RationalNumber, n::Integer)
    if n < 0
        RationalNumber(den(r) ^ -n, num(r) ^ -n)
    else
        RationalNumber(num(r) ^ n, den(r) ^ n)
    end
end

function ^(n::Integer, r::RationalNumber)
    n ^ (num(r) / den(r))
end

# absolute value
function abs(r::RationalNumber)
    RationalNumber(abs(num(r)), den(r))
end

# comparison operators
function ==(a::RationalNumber, b::RationalNumber)
    num(a) == num(b) && den(a) == den(b)
end
==(a::RationalNumber, b::Integer) = a == RationalNumber(b)

!=(a::RationalNumber, b::RationalNumber) = !(a == b)
!=(a::RationalNumber, b::Integer) = a != RationalNumber(b)

function <(a::RationalNumber, b::RationalNumber)
    float(a) < float(b)    
end
<(a::RationalNumber, b::Integer) = a < RationalNumber(b)

function <=(a::RationalNumber, b::RationalNumber)
    float(a) <= float(b)    
end
<=(a::RationalNumber, b::Integer) = a <= RationalNumber(b)

>(a::RationalNumber, b::RationalNumber) = !(a <= b)
>(a::RationalNumber, b::Integer)        = !(a <= b)

>=(a::RationalNumber, b::RationalNumber) = !(a < b)
>=(a::RationalNumber, b::Integer)        = !(a < b)
