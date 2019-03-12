local Allergens = {
    'eggs', 'peanuts', 'shellfish', 'strawberries',
    'tomatoes', 'chocolate', 'pollen', 'cats',
}
-- and the inverse mapping
local AllergenScore = {}
for i, a in ipairs(Allergens) do
    AllergenScore[a] = 1 << (i-1)
end

-----------------------------------------------------

local allergic_to = function(code, allergen)
    assert(AllergenScore[allergen], 'Unknown allergen')
    return code & AllergenScore[allergen] > 0
end

local list = function(code)
    local allergens = {}
    for i,allergen in ipairs(Allergens) do
        if allergic_to(code, allergen) then
            allergens[#allergens+1] = allergen
        end
    end
    return allergens
end

return {
    allergic_to = allergic_to,
    list = list,
}
