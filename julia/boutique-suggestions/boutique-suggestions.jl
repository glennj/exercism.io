function clothingitem(categories, qualities)
    Dict(c => q for (c, q) in zip(categories, qualities))
end

function get_combinations(tops, bottoms)
    [(t, b) for t in tops, b in bottoms]

end

function get_prices(combos)
    
end

function filter_clashing(combos)

end
