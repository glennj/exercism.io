import ballerina/test;

@test:Config
function squareOfSum_1() {
    test:assertEquals(1, squareOfSum(1));
}

@test:Config {
    //enable: false
}
function squareOfSum_5() {
    test:assertEquals(225, squareOfSum(5));
}

@test:Config {
    //enable: false
}
function squareOfSum_100() {
    test:assertEquals(25502500, squareOfSum(100));
}

@test:Config {
    //enable: false
}
function sumOfSquares_1() {
    test:assertEquals(1, sumOfSquares(1));
}

@test:Config {
    //enable: false
}
function sumOfSquares_5() {
    test:assertEquals(55, sumOfSquares(5));
}

@test:Config {
    //enable: false
}
function sumOfSquares_100() {
    test:assertEquals(338350, sumOfSquares(100));
}

@test:Config {
    //enable: false
}
function differenceOfSquares_1() {
    test:assertEquals(0, differenceOfSquares(1));
}

@test:Config {
    //enable: false
}
function differenceOfSquares_5() {
    test:assertEquals(170, differenceOfSquares(5));
}

@test:Config {
    //enable: false
}
function differenceOfSquares_100() {
    test:assertEquals(25164150, differenceOfSquares(100));
}
