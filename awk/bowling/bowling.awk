@include "die"
@include "arrays"

BEGIN {
    RS = " "
    frame = 1
    frameSum = 0
    frameRolls = 0
    arrays::init(bonus)
    sum = 0
    fillBall = 0
}

{
    assert($1 >= 0, "Negative roll is invalid")
    assert(frame <= 10, "Cannot roll after game is over")
    assert(frameSum + $1 <= 10, "Pin count exceeds pins on the lane")

    frameSum += $1
    frameRolls++

    for (i = 1; i <= length(bonus); i++) {
        if (bonus[i] > 0) {
            sum += $1
            bonus[i]--
        }
    }
}

frame < 10 {
    if (frameSum == 10) {
        arrays::push(bonus, 1 + ($1 == 10))
    }
    frameComplete = (frameSum == 10 || frameRolls == 2)
}

frame == 10 {
    if (frameSum == 10 && frameRolls <= 2) {
        sum += frameSum
        frameSum = 0
        fillBall = 1
    }
    frameComplete = (frameRolls == 2 + fillBall)
}

frameComplete {
    sum += frameSum
    frame++
    frameSum = 0
    frameRolls = 0
}

END {
    assert(frame > 10, "Score cannot be taken until the end of the game")
    print sum
}
