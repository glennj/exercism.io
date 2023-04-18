import ballerina/http;
import ballerina/test;

@test:Config
function testFunc() returns error? {
    final http:Client httpEndpoint = check new ("http://localhost:9090");
    string expectedResponse = "Hello, World!";

    // Send a GET request to the specified endpoint
    string response = check httpEndpoint->/greeting;
    test:assertEquals(response, expectedResponse);
}
