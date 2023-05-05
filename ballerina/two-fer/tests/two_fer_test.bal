import ballerina/test;

@test:Config
function noNameGiven() {
    test:assertEquals("One for you, one for me.", speak(()));
}

@test:Config {
    enable: false
}
function aNameGiven() {
    test:assertEquals("One for Alice, one for me.", speak("Alice"));
}

@test:Config {
    enable: false
}
function anotherNameGiven() {
    test:assertEquals("One for Bob, one for me.", speak("Bob"));
}
