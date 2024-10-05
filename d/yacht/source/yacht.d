module yacht;

import std.algorithm : filter, sort, sum;
import std.array : array;

enum Category {
    ones,       fullHouse,
    twos,       fourOfAKind,
    threes,     littleStraight,
    fours,      bigStraight,
    fives,      yacht,
    sixes,      choice,
}

pure int score(immutable int[] dice, Category category) {
    Roll roll = Roll(dice);

    final switch (category) {
        case Category.ones:           return roll.scoreFor(1);
        case Category.twos:           return roll.scoreFor(2);
        case Category.threes:         return roll.scoreFor(3);
        case Category.fours:          return roll.scoreFor(4);
        case Category.fives:          return roll.scoreFor(5);
        case Category.sixes:          return roll.scoreFor(6);
        case Category.fullHouse:      return roll.fullHouse;
        case Category.fourOfAKind:    return roll.fourOfAKind;
        case Category.littleStraight: return roll.straight([1, 2, 3, 4, 5]);
        case Category.bigStraight:    return roll.straight([2, 3, 4, 5, 6]);
        case Category.yacht:          return roll.yacht;
        case Category.choice:         return roll.choice;
    }
}

struct Roll {
    private immutable int[] dice;
    private immutable int[] sorted;
    private immutable int choice;

    pure this(immutable int[] dice) {
        this.dice = dice.dup;
        this.sorted = dice.dup.sort.array;
        this.choice = dice.sum;
    }

    pure int yacht() => sorted[0] == sorted[4] ? 50 : 0;

    pure int scoreFor(int die) => dice.filter!((d) => d == die).sum;

    pure int straight(immutable int[] wanted) => sorted == wanted ? 30 : 0;

    pure int fourOfAKind() =>
        (sorted[0] == sorted[3] || sorted[1] == sorted[4])
            ? 4 * sorted[2]
            : 0;

    pure int fullHouse() =>
        !yacht() &&
        ((sorted[0] == sorted[1] && sorted[2] == sorted[4]) ||
         (sorted[0] == sorted[2] && sorted[3] == sorted[4])) 
            ? choice
            : 0;
}


unittest
{
    immutable int allTestsEnabled = 1;

    // Yacht
    assert(score([5, 5, 5, 5, 5], Category.yacht) == 50);

    static if (allTestsEnabled)
    {
        // Not Yacht
        assert(score([1, 3, 3, 2, 5], Category.yacht) == 0);

        // Ones
        assert(score([1, 1, 1, 3, 5], Category.ones) == 3);

        // Ones, out of order
        assert(score([3, 1, 1, 5, 1], Category.ones) == 3);

        // No ones
        assert(score([4, 3, 6, 5, 5], Category.ones) == 0);

        // Twos
        assert(score([2, 3, 4, 5, 6], Category.twos) == 2);

        // Fours
        assert(score([1, 4, 1, 4, 1], Category.fours) == 8);

        // Yacht counted as threes
        assert(score([3, 3, 3, 3, 3], Category.threes) == 15);

        // Yacht of 3s counted as fives
        assert(score([3, 3, 3, 3, 3], Category.fives) == 0);

        // Fives
        assert(score([1, 5, 3, 5, 3], Category.fives) == 10);

        // Sixes
        assert(score([2, 3, 4, 5, 6], Category.sixes) == 6);

        // Full house two small, three big
        assert(score([2, 2, 4, 4, 4], Category.fullHouse) == 16);

        // Full house three small, two big
        assert(score([5, 3, 3, 5, 3], Category.fullHouse) == 19);

        // Two pair is not a full house
        assert(score([2, 2, 4, 4, 5], Category.fullHouse) == 0);

        // Four of a kind is not a full house
        assert(score([1, 4, 4, 4, 4], Category.fullHouse) == 0);

        // Yacht is not a full house
        assert(score([2, 2, 2, 2, 2], Category.fullHouse) == 0);

        // Four of a Kind
        assert(score([6, 6, 4, 6, 6], Category.fourOfAKind) == 24);

        // Yacht can be scored as Four of a Kind
        assert(score([3, 3, 3, 3, 3], Category.fourOfAKind) == 12);

        // Full house is not Four of a Kind
        assert(score([3, 3, 3, 5, 5], Category.fourOfAKind) == 0);

        // Little Straight
        assert(score([3, 5, 4, 1, 2], Category.littleStraight) == 30);

        // Little Straight as Big Straight
        assert(score([1, 2, 3, 4, 5], Category.bigStraight) == 0);

        // Four in order but not a little straight
        assert(score([1, 1, 2, 3, 4], Category.littleStraight) == 0);

        // No pairs but not a little straight
        assert(score([1, 2, 3, 4, 6], Category.littleStraight) == 0);

        // Minimum is 1, maximum is 5, but not a little straight
        assert(score([1, 1, 3, 4, 5], Category.littleStraight) == 0);

        // Big Straight
        assert(score([4, 6, 2, 5, 3], Category.bigStraight) == 30);

        // Big Straight as little straight
        assert(score([6, 5, 4, 3, 2], Category.littleStraight) == 0);

        // No pairs but not a big straight
        assert(score([6, 5, 4, 3, 1], Category.bigStraight) == 0);

        // Choice
        assert(score([3, 3, 5, 6, 6], Category.choice) == 23);

        // Yacht as choice
        assert(score([2, 2, 2, 2, 2], Category.choice) == 10);
    }
}
