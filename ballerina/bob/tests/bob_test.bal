import ballerina/test;

@test:Config {}
function testStatingSomething() {
    string response = hey("Tom-ay-to, tom-aaaah-to.");
    string expected = "Whatever.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testShouting() {
    string response = hey("WATCH OUT!");
    string expected = "Whoa, chill out!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testShoutingGibberish() {
    string response = hey("FCECDFCAAB");
    string expected = "Whoa, chill out!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testAskingAQuestion() {
    string response = hey("Does this cryogenic chamber make me look fat?");
    string expected = "Sure.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testAskingANumericQuestion() {
    string response = hey("You are, what, like 15?");
    string expected = "Sure.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testAskingGibberish() {
    string response = hey("fffbbcbeab?");
    string expected = "Sure.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testTalkingForcefully() {
    string response = hey("Hi there!");
    string expected = "Whatever.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testUsingAcronymsInRegularSpeech() {
    string response = hey("It's OK if you don't want to go work for NASA.");
    string expected = "Whatever.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testForcefulQuestion() {
    string response = hey("WHAT'S GOING ON?");
    string expected = "Calm down, I know what I'm doing!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testShoutingNumbers() {
    string response = hey("1, 2, 3 GO!");
    string expected = "Whoa, chill out!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testNoLetters() {
    string response = hey("1, 2, 3");
    string expected = "Whatever.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testQuestionWithNoLetters() {
    string response = hey("4?");
    string expected = "Sure.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testShoutingWithSpecialCharacters() {
    string response = hey("ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!");
    string expected = "Whoa, chill out!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testShoutingWithNoExclamationMark() {
    string response = hey("I HATE THE DENTIST");
    string expected = "Whoa, chill out!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testStatementContainingQuestionMark() {
    string response = hey("Ending with ? means a question.");
    string expected = "Whatever.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testNonLettersWithQuestion() {
    string response = hey(":) ?");
    string expected = "Sure.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testPrattlingOn() {
    string response = hey("Wait! Hang on. Are you going to be OK?");
    string expected = "Sure.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testSilence() {
    string response = hey("");
    string expected = "Fine. Be that way!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testProlongedSilence() {
    string response = hey("          ");
    string expected = "Fine. Be that way!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testAlternateSilence() {
    string response = hey("\t\t\t\t\t\t\t\t\t\t");
    string expected = "Fine. Be that way!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testMultipleLineQuestion() {
    string response = hey("\nDoes this cryogenic chamber make me look fat?\nNo.");
    string expected = "Whatever.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testStartingWithWhitespace() {
    string response = hey("         hmmmmmmm...");
    string expected = "Whatever.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testEndingWithWhitespace() {
    string response = hey("Okay if like my  spacebar  quite a bit?   ");
    string expected = "Sure.";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testOtherWhitespace() {
    string response = hey("\n\r \t");
    string expected = "Fine. Be that way!";
    test:assertEquals(response, expected);
}

@test:Config {
    //////enable: false
}
function testNonQuestionEndingWithWhitespace() {
    string response = hey("This is a statement ending with whitespace      ");
    string expected = "Whatever.";
    test:assertEquals(response, expected);
}
