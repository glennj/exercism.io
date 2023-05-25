import ballerina/test;

@test:Config
function testWinkFor1() {
    string[] result = commands(1);
    string[] expected = ["wink"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testDoubleBlinkFor10() {
    string[] result = commands(2);
    string[] expected = ["double blink"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCloseYourEyesFor100() {
    string[] result = commands(4);
    string[] expected = ["close your eyes"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testJumpFor1000() {
    string[] result = commands(8);
    string[] expected = ["jump"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCombineTwoActions() {
    string[] result = commands(3);
    string[] expected = ["wink", "double blink"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testReverseTwoActions() {
    string[] result = commands(19);
    string[] expected = ["double blink", "wink"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testReversingOneActionGivesTheSameAction() {
    string[] result = commands(24);
    string[] expected = ["jump"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testReversingNoActionsStillGivesNoActions() {
    string[] result = commands(16);
    string[] expected = [];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testAllPossibleActions() {
    string[] result = commands(15);
    string[] expected = ["wink", "double blink", "close your eyes", "jump"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testReverseAllPossibleActions() {
    string[] result = commands(31);
    string[] expected = ["jump", "close your eyes", "double blink", "wink"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testDoNothingForZero() {
    string[] result = commands(0);
    string[] expected = [];
    test:assertEquals(result, expected);
}
