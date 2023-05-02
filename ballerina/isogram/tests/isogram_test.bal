import ballerina/test;

@test:Config
function EmptyString() {
    test:assertTrue(isIsogram(""));
}

@test:Config
function IsogramWithOnlyLowerCaseCharacters() {
    test:assertTrue(isIsogram("isogram"));
}

@test:Config
function WordWithOneDuplicatedCharacter() {
    test:assertFalse(isIsogram("eleven"));
}

@test:Config
function WordWithOneDuplicatedCharacterFromTheEndOfTheAlphabet() {
    test:assertFalse(isIsogram("zzyzx"));
}

@test:Config
function LongestReportedEnglishIsogram() {
    test:assertTrue(isIsogram("subdermatoglyphic"));
}

@test:Config
function WordWithDuplicatedCharacterInMixedCase() {
    test:assertFalse(isIsogram("Alphabet"));
}

@test:Config
function WordWithDuplicatedCharacterInMixedCaseLowercaseFirst() {
    test:assertFalse(isIsogram("alphAbet"));
}

@test:Config
function HypotheticalIsogrammicWordWithHyphen() {
    test:assertTrue(isIsogram("thumbscrew-japingly"));
}

@test:Config
function HypotheticalWordWithDuplicatedCharacterFollowingHyphen() {
    test:assertFalse(isIsogram("thumbscrew-jappingly"));
}

@test:Config
function IsogramWithDuplicatedHyphen() {
    test:assertTrue(isIsogram("six-year-old"));
}

@test:Config
function MadeUpNameThatIsAnIsogram() {
    test:assertTrue(isIsogram("Emily Jung Schwartzkopf"));
}

@test:Config
function DuplicatedCharacterInTheMiddle() {
    test:assertFalse(isIsogram("accentor"));
}

@test:Config
function SameFirstAndLastCharacters() {
    test:assertFalse(isIsogram("angola"));
}

@test:Config
function WordWithDuplicatedCharacterAndWithTwoHyphens() {
    test:assertFalse(isIsogram("up-to-date"));
}
