import Base: isempty, in, length, iterate, push!, filter!

struct CustomSet{T} <: AbstractSet{T}
    elements::Array{T,1}
    CustomSet(a::Array{T,1}) where T = new{T}(unique(a))
end

# unneeded constructor methods.
#CustomSet{T}() where {T} = CustomSet(Array{T,1}())
#CustomSet{T}(a::CustomSet{T}) where {T} = CustomSet(a.elements)

isempty(a::CustomSet) = isempty(a.elements)

length(a::CustomSet) = length(a.elements)

in(elem, a::CustomSet) = elem in a.elements

filter!(f::Any, a::CustomSet) = filter!(f, a.elements)

function issubset(a::CustomSet, b::CustomSet) 
    all(elem -> elem in b, a.elements)
end

function disjoint(a::CustomSet, b::CustomSet) 
    !any(elem -> elem in b, a.elements)
end

iterate(a::CustomSet) = iterate(a.elements)
function iterate(a::CustomSet, state::Union{Nothing, T}) where T
    iterate(a.elements, state)
end

function push!(a::CustomSet, elem)
    if !(elem in a)
        push!(a.elements, elem)
    end
end

function complement!(a::CustomSet, b::CustomSet)
    filter!(elem -> !(elem in b), a)
end
function complement(a::CustomSet, b::CustomSet)
    CustomSet(filter(elem -> !(elem in b), a.elements))
end

function copy(a::CustomSet)
    CustomSet(a.elements)
end
