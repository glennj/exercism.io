import ballerina/test;

@test:Config {}
function testBasic() {
    test:assertEquals(abbreviate("Portable Network Graphics"), "PNG");
}

@test:Config {
    //////////////////enable: false
}
function testLowercaseWords() {
    test:assertEquals(abbreviate("Ruby on Rails"), "ROR");
}

@test:Config {
    //////////////////enable: false
}
function testPunctuation() {
    test:assertEquals(abbreviate("First In, First Out"), "FIFO");
}

@test:Config {
    //////////////////enable: false
}
function testAllCapsWord() {
    test:assertEquals(abbreviate("GNU Image Manipulation Program"), "GIMP");
}

@test:Config {
    //////////////////enable: false
}
function testPunctuationWithoutWhitespace() {
    test:assertEquals(abbreviate("Complementary metal-oxide semiconductor"), "CMOS");
}

@test:Config {
    //////////////////enable: false
}
function testVeryLongAbbreviation() {
    test:assertEquals(abbreviate("Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"), "ROTFLSHTMDCOALM");
}

@test:Config {
    //////////////////enable: false
}
function testConsecutiveDelimiters() {
    test:assertEquals(abbreviate("Something - I made up from thin air"), "SIMUFTA");
}

@test:Config {
    //////////////////enable: false
}
function testApostrophes() {
    test:assertEquals(abbreviate("Halley's Comet"), "HC");
}

@test:Config {
    //////////////////enable: false
}
function testUnderscoreEmphasis() {
    test:assertEquals(abbreviate("The Road _Not_ Taken"), "TRNT");
}
