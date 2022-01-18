import Base: show, isvalid

struct ISBN <: AbstractString
    isbn::AbstractString

    function ISBN(s::AbstractString)
        isvalid(ISBN, s) || throw(DomainError(s))
        new(replace(s, "-" => ""))
    end
end

show(io::IO, isbn::ISBN) = show(io, isbn.isbn)

function isvalid(::Type{T}, s::AbstractString) where T<:ISBN
    occursin(r"^[\d-]+[\dX]$", s) || return false

    ds = [
        (s[end] == 'X' ? 10 : parse(Int, s[end]));
        digits(parse(Int, filter(isdigit, s[1:end-1])))
    ]
    length(ds) == 10 || return false

    sum(i * d for (i, d) in enumerate(ds)) % 11 == 0
end

macro isbn_str(s::AbstractString)
    ISBN(s).isbn
end
