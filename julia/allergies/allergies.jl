const allergens = Dict(
    "eggs"         =>   1,
    "peanuts"      =>   2,
    "shellfish"    =>   4,
    "strawberries" =>   8,
    "tomatoes"     =>  16,
    "chocolate"    =>  32,
    "pollen"       =>  64,
    "cats"         => 128,
)

function allergic_to(score, allergen) 
    haskey(allergens, allergen) && 
    score & allergens[allergen] != 0
end

function allergy_list(score) 
    filter(a -> allergic_to(score, a), keys(allergens))
end
