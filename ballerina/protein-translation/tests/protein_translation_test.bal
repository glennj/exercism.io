import ballerina/test;

@test:Config
function testEmptyRNASequenceResultsInNoProteins() {
    string[]|error result = proteins("");
    string[] expected = [];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testMethionineRNASequence() {
    string[]|error result = proteins("AUG");
    string[] expected = ["Methionine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testPhenylalanineRNASequence1() {
    string[]|error result = proteins("UUU");
    string[] expected = ["Phenylalanine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testPhenylalanineRNASequence2() {
    string[]|error result = proteins("UUC");
    string[] expected = ["Phenylalanine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testLeucineRNASequence1() {
    string[]|error result = proteins("UUA");
    string[] expected = ["Leucine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testLeucineRNASequence2() {
    string[]|error result = proteins("UUG");
    string[] expected = ["Leucine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSerineRNASequence1() {
    string[]|error result = proteins("UCU");
    string[] expected = ["Serine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSerineRNASequence2() {
    string[]|error result = proteins("UCC");
    string[] expected = ["Serine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSerineRNASequence3() {
    string[]|error result = proteins("UCA");
    string[] expected = ["Serine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSerineRNASequence4() {
    string[]|error result = proteins("UCG");
    string[] expected = ["Serine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTyrosineRNASequence1() {
    string[]|error result = proteins("UAU");
    string[] expected = ["Tyrosine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTyrosineRNASequence2() {
    string[]|error result = proteins("UAC");
    string[] expected = ["Tyrosine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCysteineRNASequence1() {
    string[]|error result = proteins("UGU");
    string[] expected = ["Cysteine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCysteineRNASequence2() {
    string[]|error result = proteins("UGC");
    string[] expected = ["Cysteine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTryptophanRNASequence() {
    string[]|error result = proteins("UGG");
    string[] expected = ["Tryptophan"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSTOPCodonRNASequence1() {
    string[]|error result = proteins("UAA");
    string[] expected = [];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSTOPCodonRNASequence2() {
    string[]|error result = proteins("UAG");
    string[] expected = [];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSTOPCodonRNASequence3() {
    string[]|error result = proteins("UGA");
    string[] expected = [];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSequenceOfTwoProteinCodonsTranslatesIntoProteins() {
    string[]|error result = proteins("UUUUUU");
    string[] expected = ["Phenylalanine", "Phenylalanine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testSequenceOfTwoDifferentProteinCodonsTranslatesIntoProteins() {
    string[]|error result = proteins("UUAUUG");
    string[] expected = ["Leucine", "Leucine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTranslateRNAStrandIntoCorrectProteinList() {
    string[]|error result = proteins("AUGUUUUGG");
    string[] expected = ["Methionine", "Phenylalanine", "Tryptophan"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTranslationStopsIfSTOPCodonAtBeginningOfSequence() {
    string[]|error result = proteins("UAGUGG");
    string[] expected = [];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTranslationStopsIfSTOPCodonAtEndOfTwoCodonSequence() {
    string[]|error result = proteins("UGGUAG");
    string[] expected = ["Tryptophan"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTranslationStopsIfSTOPCodonAtEndOfThreeCodonSequence() {
    string[]|error result = proteins("AUGUUUUAA");
    string[] expected = ["Methionine", "Phenylalanine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTranslationStopsIfSTOPCodonInMiddleOfThreeCodonSequence() {
    string[]|error result = proteins("UGGUAGUGG");
    string[] expected = ["Tryptophan"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testTranslationStopsIfSTOPCodonInMiddleOfSixCodonSequence() {
    string[]|error result = proteins("UGGUGUUAUUAAUGGUUU");
    string[] expected = ["Tryptophan", "Cysteine", "Tyrosine"];
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testNonExistingCodonCannotTranslate() {
    string[]|error result = proteins("AAA");
    if result is error {
        test:assertEquals(result.message(), "Invalid codon");
    } else {
        test:assertFail("Expected an error");
    }
}

@test:Config {
    //enable: false
}
function testUnknownAminoAcidsNotPartOfACodonCannotTranslate() {
    string[]|error result = proteins("XYZ");
    if result is error {
        test:assertEquals(result.message(), "Invalid codon");
    } else {
        test:assertFail("Expected an error");
    }
}

@test:Config {
    //enable: false
}
function testIncompleteRNASequenceCannotTranslate() {
    string[]|error result = proteins("AUGU");
    if result is error {
        test:assertEquals(result.message(), "Invalid codon");
    } else {
        test:assertFail("Expected an error");
    }
}

@test:Config {
    //enable: false
}
function testIncompleteRNASequenceCanTranslateIfValidUntilASTOPCodon() {
    string[]|error result = proteins("UUCUUCUAAUGGU");
    string[] expected = ["Phenylalanine", "Phenylalanine"];
    test:assertEquals(result, expected);
}
