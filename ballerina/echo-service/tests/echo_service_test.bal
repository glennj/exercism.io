import ballerina/http;
import ballerina/test;

final http:Client testClient = check new ("http://localhost:8080");

@test:Config
public function testEcho() returns error? {
    string payload = "Hello";
    string response = check testClient->get("/echo?sound=" + payload);
    test:assertEquals(response, payload);
}

@test:Config
public function testEchoDefinition() returns error? {
    string response = check testClient->get("/echo/definition");
    test:assertEquals(response, "A sound or series of sounds caused by the reflection of sound waves from a surface back to the listener.");
}
