# WIP
# failing with `ERROR: LoadError: LoadError: UndefVarError: T not defined`
# why fail: this is how julia defines a Set
# https://github.com/JuliaLang/julia/blob/55e36cc308b66d3472990a06b2797f9f9154ea0a/base/set.jl

struct CustomSet{T} <: AbstractSet{T}
    elements::Array{T,1}

    CustomSet{T}() where {T} = new(Array{T,1}())
    CustomSet{T}(container::Array{T,1}) = new(x for x in container)
    CustomSet{T}(cs::CustomSet{T})      = new(x for x in cs.elements)
end

isempty(cs::CustomSet) = isempty(cs.elements)

in(elem, cs::CustomSet) = elem in cs.elements

length(cs::CustomSet) = length(cs.elements)

push!(cs::CustomSet, elem) = push!(cs.elements, elem)

function pop!(cs::CustomSet, elem) 
    pop!(cs.elements, elem)
    elem
end

function pop!(cs::CustomSet, elem, default) 
    if elem in cs
        pop!(cs, elem)
    else
        default
    end
end


function copy(cs::CustomSet)
    CustomSet(cs.elements)
end
