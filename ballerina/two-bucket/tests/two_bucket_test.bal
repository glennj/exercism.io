import ballerina/test;

@test:Config
function testMeasureUsingBucketOneOfSize3AndBucketTwoOfSize5StartWithBucketOne() {
    TwoBucketSolution|error result = measure(
        bucketOne = 3,
        bucketTwo = 5,
        goal = 1,
        startBucket = "one"
    );
    TwoBucketSolution expected = {moves: 4, goalBucket: "one", otherBucket: 5};
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testMeasureUsingBucketOneOfSize3AndBucketTwoOfSize5StartWithBucketTwo() {
    TwoBucketSolution|error result = measure(
        bucketOne = 3,
        bucketTwo = 5,
        goal = 1,
        startBucket = "two"
    );
    TwoBucketSolution expected = {moves: 8, goalBucket: "two", otherBucket: 3};
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testMeasureUsingBucketOneOfSize7AndBucketTwoOfSize11StartWithBucketOne() {
    TwoBucketSolution|error result = measure(
        bucketOne = 7,
        bucketTwo = 11,
        goal = 2,
        startBucket = "one"
    );
    TwoBucketSolution expected = {moves: 14, goalBucket: "one", otherBucket: 11};
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testMeasureUsingBucketOneOfSize7AndBucketTwoOfSize11StartWithBucketTwo() {
    TwoBucketSolution|error result = measure(
        bucketOne = 7,
        bucketTwo = 11,
        goal = 2,
        startBucket = "two"
    );
    TwoBucketSolution expected = {moves: 18, goalBucket: "two", otherBucket: 7};
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testMeasureOneStepUsingBucketOneOfSize1AndBucketTwoOfSize3StartWithBucketTwo() {
    TwoBucketSolution|error result = measure(
        bucketOne = 1,
        bucketTwo = 3,
        goal = 3,
        startBucket = "two"
    );
    TwoBucketSolution expected = {moves: 1, goalBucket: "two", otherBucket: 0};
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testMeasureUsingBucketOneOfSize2AndBucketTwoOfSize3StartWithBucketOneAndEndWithBucketTwo() {
    TwoBucketSolution|error result = measure(
        bucketOne = 2,
        bucketTwo = 3,
        goal = 3,
        startBucket = "one"
    );
    TwoBucketSolution expected = {moves: 2, goalBucket: "two", otherBucket: 2};
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testNotPossibleToReachTheGoal() {
    TwoBucketSolution|error result = measure(
        bucketOne = 6,
        bucketTwo = 15,
        goal = 5,
        startBucket = "one"
    );

    if result is error {
        test:assertEquals(result.message(), "goal is impossible");
    } else {
        test:assertFail("should have been an error");
    }
}

@test:Config {
    //enable: false
}
function testWithTheSameBucketsButADifferentGoalThenItIsPossible() {
    TwoBucketSolution|error result = measure(
        bucketOne = 6,
        bucketTwo = 15,
        goal = 9,
        startBucket = "one"
    );
    TwoBucketSolution expected = {moves: 10, goalBucket: "two", otherBucket: 0};
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testGoalLargerThanBothBucketsIsImpossible() {
    TwoBucketSolution|error result = measure(
        bucketOne = 5,
        bucketTwo = 7,
        goal = 8,
        startBucket = "one"
    );

    if result is error {
        test:assertEquals(result.message(), "goal is impossible");
    } else {
        test:assertFail("should have been an error");
    }
}
