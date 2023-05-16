import ballerina/test;

@test:Config {}
function testFindsTheLargestProductIfSpanEqualsLength() {
    var result = largestProduct("29", 2);
    var expected = 18;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCanFindTheLargestProductOf2WithNumbersInOrder() {
    var result = largestProduct("0123456789", 2);
    var expected = 72;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCanFindTheLargestProductOf2() {    
    var result = largestProduct("576802143", 2);
    var expected = 48;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCanFindTheLargestProductOf3WithNumbersInOrder() {  
    var result = largestProduct("0123456789", 3);
    var expected = 504;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCanFindTheLargestProductOf3() {    
    var result = largestProduct("1027839564", 3);
    var expected = 270;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCanFindTheLargestProductOf5WithNumbersInOrder() {  
    var result = largestProduct("0123456789", 5);
    var expected = 15120;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testCanGetTheLargestProductOfABigNumber() {    
    var result = largestProduct("73167176531330624919225119674426574742355349194934", 6);
    var expected = 23520;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testReportsZeroIfTheOnlyDigitsAreZero() {  
    var result = largestProduct("0000", 2);
    var expected = 0;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testReportsZeroIfAllSpansIncludeZero() {   
    var result = largestProduct("99099", 3);
    var expected = 0;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testRejectsSpanLongerThanStringLength() {
    var result = largestProduct("123", 4);
    if result is error {
        test:assertEquals(result.message(), "span must be smaller than string length");
    } else {
        test:assertFail("expected an error");
    }
}

@test:Config {
    //enable: false
}
function testReports1ForEmptyStringAndEmptyProduct0Span() {
    var result = largestProduct("", 0); 
    var expected = 1;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testReports1ForNonemptyStringAndEmptyProduct0Span() {  
    var result = largestProduct("123", 0);
    var expected = 1;
    test:assertEquals(result, expected);
}

@test:Config {
    //enable: false
}
function testRejectsEmptyStringAndNonzeroSpan() {
    var result = largestProduct("", 1);
    if result is error {
        test:assertEquals(result.message(), "span must be smaller than string length");
    } else {
        test:assertFail("expected an error");
    }
}

@test:Config {
    //enable: false
}
function testRejectsInvalidCharacterInDigits() {
    var result = largestProduct("1234a5", 2);
    if result is error {
        test:assertEquals(result.message(), "digits input must only contain digits");
    } else {
        test:assertFail("expected an error");
    }
}

@test:Config {
    //enable: false
}
function testRejectsNegativeSpan() {
    var result = largestProduct("12345", -1);
    if result is error {
        test:assertEquals(result.message(), "span must not be negative");
    } else {
        test:assertFail("expected an error");
    }
}
