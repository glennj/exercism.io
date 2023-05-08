type Bucket record {
    string name;
    int size;
    int amount;
};

# Determine how many actions are required to measure an exact number of liters
# by strategically transferring fluid between the buckets.
#
# + bucketOne - the size of the first bucket
# + bucketTwo - the size of the second bucket
# + goal - the desired number of litres to reach 
# + startBucket - the name of the first bucket to fill
# + return - a TestBucketResult record or an error
public function measure(int bucketOne, int bucketTwo, int goal, string startBucket) returns TwoBucketSolution|error {
    if !valid(bucketOne, bucketTwo, goal) {
        return error("goal is impossible");
    }
    [Bucket, Bucket] [first, second] = initBuckets(bucketOne, bucketTwo, startBucket);
    return solve(first, second, goal);
}

function valid(int size1, int size2, int goal) returns boolean {
    if goal > int:max(size1, size2) {
        return false;
    }
    int g = gcd(size1, size2);
    return g == 1 || goal % g == 0;
}

function gcd(int a, int b) returns int {
    if b == 0 {
        return a;
    }
    return gcd(b, a % b);
};

function initBuckets(int size1, int size2, string startBucket) returns [Bucket, Bucket] {
    Bucket one = {name: "one", size: size1, amount: 0};
    Bucket two = {name: "two", size: size2, amount: 0};
    return startBucket == "one" ? [one, two] : [two, one];
}

function solve(Bucket a, Bucket b, int goal) returns TwoBucketSolution {
    [Bucket, Bucket] [first, second] = [a, b];

    int moves = 0;

    first.amount = first.size;
    moves += 1;

    if second.size == goal {
        second.amount = second.size;
        moves += 1;
    }

    while true {
        if first.amount == goal {
            return {moves, goalBucket: first.name, otherBucket: second.amount};
        }
        if second.amount == goal {
            return {moves, goalBucket: second.name, otherBucket: first.amount};
        }

        // if first is empty, fill it
        // else if second is full, empty it
        // else pour from first into second

        if first.amount == 0 {
            first.amount = first.size;
        }
        else if second.amount == second.size {
            second.amount = 0;
        }
        else {
            int quantity = int:min(first.amount, second.size - second.amount);
            first.amount -= quantity;
            second.amount += quantity;
        }

        moves += 1;
    }
}
