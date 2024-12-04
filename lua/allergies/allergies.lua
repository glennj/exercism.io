local Allergens = {
    'eggs', 'peanuts', 'shellfish', 'strawberries',
    'tomatoes', 'chocolate', 'pollen', 'cats',
}

local list = function(code)
    local allergens = {}
    for i, allergen in ipairs(Allergens) do
        if (code >> i-1) & 1 == 1 then
            allergens[#allergens+1] = allergen
        end
    end
    return allergens
end

local allergic_to = function(code, allergen)
    for _, a in ipairs(list(code)) do
        if a == allergen then return true end
    end
    return false
end

return {
    allergic_to = allergic_to,
    list = list,
}
