import ballerina/test;

@test:Config
function testNoMultiplesWithinLimit() {
    int actual = sum([3, 5], 1);
    int expected = 0;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testOneFactorHasMultiplesWithinLimit() {
    int actual = sum([3, 5], 4);
    int expected = 3;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testMoreThanOneMultipleWithinLimit() {
    int actual = sum([3], 7);
    int expected = 9;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testMoreThanOneFactorWithMultiplesWithinLimit() {
    int actual = sum([3, 5], 10);
    int expected = 23;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testEachMultipleIsOnlyCountedOnce() {
    int actual = sum([3, 5], 100);
    int expected = 2318;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testAMuchLargerLimit() {
    int actual = sum([3, 5], 1000);
    int expected = 233168;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testThreeFactors() {
    int actual = sum([7, 13, 17], 20);
    int expected = 51;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testFactorsNotRelativelyPrime() {
    int actual = sum([4, 6], 15);
    int expected = 30;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testSomePairsOfFactorsRelativelyPrimeAndSomeNot() {
    int actual = sum([5, 6, 8], 150);
    int expected = 4419;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testOneFactorIsAMultipleOfAnother() {
    int actual = sum([5, 25], 51);
    int expected = 275;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testMuchLargerFactors() {
    int actual = sum([43, 47], 10000);
    int expected = 2203160;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testAllNumbersAreMultiplesOf1() {
    int actual = sum([1], 100);
    int expected = 4950;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testNoFactorsMeansAnEmptySum() {
    int actual = sum([], 10000);
    int expected = 0;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testTheOnlyMultipleOf0Is0() {
    int actual = sum([0], 1);
    int expected = 0;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testTheFactor0DoesNotAffectTheSumOfMultiplesOfOtherFactors() {
    int actual = sum([3, 0], 4);
    int expected = 3;
    test:assertEquals(actual, expected);
}

@test:Config {
    //enable: false
}
function testSolutionsUsingIncludeExcludeMustExtendToCardinalityGreaterThan3() {
    int actual = sum([2, 3, 5, 7, 11], 10000);
    int expected = 39614537;
    test:assertEquals(actual, expected);
}
