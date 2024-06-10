-- forward declarations for local functions
local single, full_house, four_of_a_kind, straight, yacht, choice, groups

-- ------------------------------------------------------------
local Yacht = {
    score = function(dice, category)
        if category == 'ones'            then return  single(dice, 1)      end
        if category == 'twos'            then return  single(dice, 2)      end
        if category == 'threes'          then return  single(dice, 3)      end
        if category == 'fours'           then return  single(dice, 4)      end
        if category == 'fives'           then return  single(dice, 5)      end
        if category == 'sixes'           then return  single(dice, 6)      end
        if category == 'full house'      then return  full_house(dice)     end
        if category == 'four of a kind'  then return  four_of_a_kind(dice) end
        if category == 'little straight' then return  straight(dice, 0)    end
        if category == 'big straight'    then return  straight(dice, 1)    end
        if category == 'yacht'           then return  yacht(dice)          end
        if category == 'choice'          then return  choice(dice)         end
        return 0
    end
}

-- ------------------------------------------------------------
function single(dice, wanted)
    local s = 0
    for _, die in ipairs(dice) do
        if die == wanted then
            s = s + die
        end
    end
    return s
end

function full_house(dice)
    local h = {}
    local sum = 0
    for die, count in ipairs(groups(dice)) do
        if count > 0 then
            h[#h + 1] = count
        end
        sum = sum + die * count
    end
    return (#h == 2 and (h[1] == 2 or h[1] == 3)) and sum or 0
end

function four_of_a_kind(dice)
    for die, count in ipairs(groups(dice)) do
        if count >= 4 then
            return 4 * die
        end
    end
    return 0
end

function straight(dice, offset)
    table.sort(dice)
    for i = 1, #dice do
        if dice[i] ~= i + offset then
            return 0
        end
    end
    return 30
end

function yacht(dice)
    for _, count in ipairs(groups(dice)) do
        if count == 5 then
            return 50
        end
    end
    return 0
end

function choice(dice)
    local s = 0
    for i = 1, #dice do
        s = s + dice[i]
    end
    return s
end

function groups(dice)
    local g = {[1]=0, [2]=0, [3]=0, [4]=0, [5]=0, [6]=0}
    for _, die in ipairs(dice) do
        g[die] = g[die] + 1
    end
    return g
end

-- ------------------------------------------------------------
return Yacht
