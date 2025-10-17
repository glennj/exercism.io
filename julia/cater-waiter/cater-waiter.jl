function clean_ingredients(dish_name, dish_ingredients)
    (dish_name, Set(dish_ingredients))
end

function check_drinks(drink_name, drink_ingredients)
    # drink_type = isempty(drink_ingredients ∩ ALCOHOLS) ? "Mocktail" : "Cocktail"
    drink_type = isdisjoint(drink_ingredients, ALCOHOLS) ? "Mocktail" : "Cocktail"
    "$drink_name $drink_type"
end

function categorize_dish(dish_name, dish_ingredients)
    #=
    dish_category = "OMNIVORE"
    categories = ["VEGAN" => VEGAN, "VEGETARIAN" => VEGETARIAN, "PALEO" => PALEO, "KETO" => KETO]
    for (category, ingredients) in categories
        if issubset(dish_ingredients, ingredients)
            dish_category = category
            break
        end
    end
    "$dish_name: $dish_category"
    =#

    # This version plays with symbols:
    for category in [:VEGAN, :VEGETARIAN, :PALEO, :KETO, :OMNIVORE]
        if issubset(dish_ingredients, eval(category))
            return dish_name * ": " * String(category)
        end
    end

end

function tag_special_ingredients((dish_name, dish_ingredients))
    (dish_name, Set(dish_ingredients) ∩ SPECIAL_INGREDIENTS)
end

function compile_ingredients(dishes)
    #= hmmm, according to https://docs.julialang.org/en/v1/base/collections/#Base.union
     # this should work:
    union(Set(), dishes)
    =#
    
    #=
    result = Set()
    for dish in dishes
        union!(result, dish)
    end
    result
    =#

    # with a higher-order function
    #foldl(union!, dishes; init=Set())

    # aha! the dishes parameter is a collection of collections.
    # I just need to splat them apart.
    #union(Set(), dishes...)

    # Don't even need to include the empty set
    union(dishes...)
end

function separate_appetizers(dishes, appetizers)
    setdiff(Set(dishes), Set(appetizers)) |> collect
end

# Return a set of ingredients appearing in only one dish in the category.
# `intersection` is a set of ingredients that appear in more than one dish in the category.
function singleton_ingredients(dishes, intersection)
    singleton_ingredients_per_dish = [setdiff(dish, intersection) for dish in dishes]
    # then, accumulating, discard ingredients that 2 sets have in common.
    reduce(symdiff!, singleton_ingredients_per_dish)

    # as Colin writes in https://exercism.org/tracks/julia/exercises/cater-waiter/solutions/colinleach
    # this can be simpler: combine all the ingredients of all the dishes, then remove the known duplicates.
    #setdiff(union(dishes...), intersection)
end
