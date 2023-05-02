import ballerina/test;

@test:Config {}
function test1isI() {
    test:assertEquals(roman(1), "I");
}

@test:Config {}
function test2isII() {
    test:assertEquals(roman(2), "II");
}

@test:Config {}
function test3isIII() {
    test:assertEquals(roman(3), "III");
}

@test:Config {}
function test4isIV() {
    test:assertEquals(roman(4), "IV");
}

@test:Config {}
function test5isV() {
    test:assertEquals(roman(5), "V");
}

@test:Config {}
function test6isVI() {
    test:assertEquals(roman(6), "VI");
}

@test:Config {}
function test9isIX() {
    test:assertEquals(roman(9), "IX");
}

@test:Config {}
function test16isXVI() {
    test:assertEquals(roman(16), "XVI");
}

@test:Config {}
function test27isXXVII() {
    test:assertEquals(roman(27), "XXVII");
}

@test:Config {}
function test48isXLVIII() {
    test:assertEquals(roman(48), "XLVIII");
}

@test:Config {}
function test49isXLIX() {
    test:assertEquals(roman(49), "XLIX");
}

@test:Config {}
function test59isLIX() {
    test:assertEquals(roman(59), "LIX");
}

@test:Config {}
function test66isLXVI() {
    test:assertEquals(roman(66), "LXVI");
}

@test:Config {}
function test93isXCIII() {
    test:assertEquals(roman(93), "XCIII");
}

@test:Config {}
function test141isCXLI() {
    test:assertEquals(roman(141), "CXLI");
}

@test:Config {}
function test163isCLXIII() {
    test:assertEquals(roman(163), "CLXIII");
}

@test:Config {}
function test166isCLXVI() {
    test:assertEquals(roman(166), "CLXVI");
}

@test:Config {}
function test402isCDII() {
    test:assertEquals(roman(402), "CDII");
}

@test:Config {}
function test575isDLXXV() {
    test:assertEquals(roman(575), "DLXXV");
}

@test:Config {}
function test666isDCLXVI() {
    test:assertEquals(roman(666), "DCLXVI");
}

@test:Config {}
function test911isCMXI() {
    test:assertEquals(roman(911), "CMXI");
}

@test:Config {}
function test1024isMXXIV() {
    test:assertEquals(roman(1024), "MXXIV");
}

@test:Config {}
function test1666isMDCLXVI() {
    test:assertEquals(roman(1666), "MDCLXVI");
}

@test:Config {}
function test3000isMMM() {
    test:assertEquals(roman(3000), "MMM");
}

@test:Config {}
function test3001isMMMI() {
    test:assertEquals(roman(3001), "MMMI");
}

@test:Config {}
function test3999isMMMCMXCIX() {
    test:assertEquals(roman(3999), "MMMCMXCIX");
}
