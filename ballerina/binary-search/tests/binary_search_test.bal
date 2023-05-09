import ballerina/test;

@test:Config
function findsAValueInAnArrayWithOneElement() {
    int? result = find([6], 6);
    int expected = 0;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function findsAValueInTheMiddleOfAnArray() {
    int? result = find([1, 3, 4, 6, 8, 9, 11], 6);
    int expected = 3;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function findsAValueAtTheBeginningOfAnArray() {
    int? result = find([1, 3, 4, 6, 8, 9, 11], 1);
    int expected = 0;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function findsAValueAtTheEndOfAnArray() {
    int? result = find([1, 3, 4, 6, 8, 9, 11], 11);
    int expected = 6;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function findsAValueInAnArrayOfOddLength() {
    int? result = find([1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 634], 144);
    int expected = 9;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function findsAValueInAnArrayOfEvenLength() {
    int? result = find([1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377], 21);
    int expected = 5;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function identifiesThatAValueIsNotIncludedInTheArray() {
    int? result = find([1, 3, 4, 6, 8, 9, 11], 7);
    test:assertEquals(result, ());
}

@test:Config {
    //enable: false
}
function aValueSmallerThanTheArray() {
    int? result = find([1, 3, 4, 6, 8, 9, 11], 0);
    test:assertEquals(result, ());
}

@test:Config {
    //enable: false
}
function aValueLargerThanTheArray() {
    int? result = find([1, 3, 4, 6, 8, 9, 11], 13);
    test:assertEquals(result, ());
}

@test:Config {
    //enable: false
}
function nothingIsFoundInAnEmptyArray() {
    int? result = find([], 1);
    test:assertEquals(result, ());
}

@test:Config {
    //enable: false
}
function nothingIsFoundWhenTheLeftAndRightBoundsCross() {
    int? result = find([1, 2], 0);
    test:assertEquals(result, ());
}
