# Determine the score of a set of dice against a Yacht category.
#
# + dice - an array of 5 integers
# + category - the name of a combination of dice
# + return - the score
public function score(int[] dice, string category) returns int {
    match category {
        "choice" => { return scoreChoice(dice); }
        "yacht"  => { return scoreYacht(dice); }
        "ones"   => { return scoreDie(dice, 1); }
        "twos"   => { return scoreDie(dice, 2); }
        "threes" => { return scoreDie(dice, 3); }
        "fours"  => { return scoreDie(dice, 4); }
        "fives"  => { return scoreDie(dice, 5); }
        "sixes"  => { return scoreDie(dice, 6); }
        "full house"      => { return scoreFullHouse(dice); }
        "four of a kind"  => { return scoreFourOfAKind(dice); }
        "little straight" => { return scoreStraight(dice, "little"); }
        "big straight"    => { return scoreStraight(dice, "big"); }
    }
    return 0;
}

type DieCount record {|
    readonly int die;
    int count;
|};

type Grouped table<DieCount> key(die);

function group(int[] dice) returns Grouped {
    Grouped grouped = table [];
    foreach int die in dice {
        DieCount? c = grouped[die];
        if c is () {
            grouped.add({die, count: 1});
        }
        else {
            c.count += 1;
            grouped.put(c);
        }
    }
    return grouped;
}

function scoreChoice(int[] dice) returns int => int:sum(...dice);

function scoreYacht(int[] dice) returns int {
    Grouped grouped = group(dice);
    return grouped.length() == 1 ? 50 : 0;
}

function scoreDie(int[] dice, int die) returns int {
    Grouped grouped = group(dice);
    DieCount? c = grouped[die];
    return c is () ? 0 : c.count * die;
}

function scoreFullHouse(int[] dice) returns int {
    Grouped grouped = group(dice);
    int[] keys = grouped.keys();
    int score = 0;
    if keys.length() == 2 {
        match grouped.get(keys[0]).count {
            2 | 3 => { score = scoreChoice(dice); }
        }
    }
    return score;
}

function scoreFourOfAKind(int[] dice) returns int {
    foreach DieCount c in group(dice) {
        if c.count == 4 || c.count == 5 {
            return 4 * c.die;
        }
    }
    return 0;
}

function scoreStraight(int[] dice, string straightType) returns int {
    match [dice.sort(), straightType] {
        [[1,2,3,4,5], "little"] => { return 30; }
        [[2,3,4,5,6], "big"]    => { return 30; }
        _ => { return 0; }
    }
}
