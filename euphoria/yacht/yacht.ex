include std/sequence.e
include std/math.e

constant true = 1

public function score(sequence roll, sequence play)
    sequence count = repeat(0, 6)
    for i = 1 to length(roll) do
        count[roll[i]] += 1
    end for

    integer total = sum(roll)

    if    equal(play, "ones")            then return single(count, 1)
    elsif equal(play, "twos")            then return single(count, 2)
    elsif equal(play, "threes")          then return single(count, 3)
    elsif equal(play, "fours")           then return single(count, 4)
    elsif equal(play, "fives")           then return single(count, 5)
    elsif equal(play, "sixes")           then return single(count, 6)
    elsif equal(play, "full house")      then return full_house(count, total)
    elsif equal(play, "four of a kind")  then return four(count)
    elsif equal(play, "little straight") then return straight(count[1..5])
    elsif equal(play, "big straight")    then return straight(count[2..6])
    elsif equal(play, "yacht")           then return yacht(count)
    elsif equal(play, "choice")          then return total
    else
        return -1
    end if
end function


function single(sequence count, integer die)
    return count[die] * die
end function

function full_house(sequence count, integer total)
    count = remove_all(0, count)
    if equal(count, {2, 3}) or equal(count, {3,2}) then
        return total
    else
        return 0
    end if
end function

function four(sequence count)
    for i = 1 to length(count) do
        if count[i] >= 4 then
            return i * 4
        end if
    end for
    return 0
end function

function straight(sequence seq)
    -- check if each element of seq is at least 1
    if equal(seq >= 1, {true,true,true,true,true}) then
        return 30
    else
        return 0
    end if
end function

function yacht(sequence count)
    for i = 1 to length(count) do
        if count[i] = 5 then
            return 50
        end if
    end for
    return 0
end function
