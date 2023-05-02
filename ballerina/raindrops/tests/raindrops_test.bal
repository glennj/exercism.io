import ballerina/test;

@test:Config
function testTheSoundFor1Is1() {
    test:assertEquals("1", convert(1));
}

@test:Config
function testTheSoundFor3IsPling() {
    test:assertEquals("Pling", convert(3));
}

@test:Config
function testTheSoundFor5IsPlang() {
    test:assertEquals("Plang", convert(5));
}

@test:Config
function testTheSoundFor7IsPlong() {
    test:assertEquals("Plong", convert(7));
}

@test:Config
function testTheSoundFor6IsPlingAsItHasAFactor3() {
    test:assertEquals("Pling", convert(6));
}

@test:Config
function testNumber2ToThePower3DoesNotMakeARaindropSoundAs3IsTheExponentNotTheBase() {
    test:assertEquals("8", convert(8));
}

@test:Config
function testTheSoundFor9IsPlingAsItHasAFactor3() {
    test:assertEquals("Pling", convert(9));
}

@test:Config
function testTheSoundFor10IsPlangAsItHasAFactor5() {
    test:assertEquals("Plang", convert(10));
}

@test:Config
function testTheSoundFor14IsPlongAsItHasAFactorOf7() {
    test:assertEquals("Plong", convert(14));
}

@test:Config
function testTheSoundFor15IsPlingplangAsItHasFactors3And5() {
    test:assertEquals("PlingPlang", convert(15));
}

@test:Config
function testTheSoundFor21IsPlingplongAsItHasFactors3And7() {
    test:assertEquals("PlingPlong", convert(21));
}

@test:Config
function testTheSoundFor25IsPlangAsItHasAFactor5() {
    test:assertEquals("Plang", convert(25));
}

@test:Config
function testTheSoundFor27IsPlingAsItHasAFactor3() {
    test:assertEquals("Pling", convert(27));
}

@test:Config
function testTheSoundFor35IsPlangplongAsItHasFactors5And7() {
    test:assertEquals("PlangPlong", convert(35));
}

@test:Config
function testTheSoundFor49IsPlongAsItHasAFactor7() {
    test:assertEquals("Plong", convert(49));
}

@test:Config
function testTheSoundFor52Is52() {
    test:assertEquals("52", convert(52));
}

@test:Config
function testTheSoundFor105IsPlingplangplongAsItHasFactors35And7() {
    test:assertEquals("PlingPlangPlong", convert(105));
}

@test:Config
function testTheSoundFor3125IsPlangAsItHasAFactor5() {
    test:assertEquals("Plang", convert(3125));
}
