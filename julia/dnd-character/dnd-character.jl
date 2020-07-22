function modifier(ability)
    # Int(floor((ability - 10) / 2))
    fld(ability - 10, 2)
end

function ability()
    rolls = rand(1:6, 4)
    sum(rolls) - minimum(rolls)
end

mutable struct DNDCharacter
    strength::Int
    dexterity::Int
    constitution::Int
    intelligence::Int
    wisdom::Int
    charisma::Int
    hitpoints::Int

    function DNDCharacter()
        abilities = [ability() for i in 1:6]
        push!(abilities, 10 + modifier(abilities[3]))
        new(abilities...)
    end
end
