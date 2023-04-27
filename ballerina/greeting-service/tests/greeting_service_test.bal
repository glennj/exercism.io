import ballerina/http;
import ballerina/test;

@test:Config
function testFunc() returns error? {
    http:Client httpEndpoint = check new ("http://localhost:9090");
    string payload = "Ballerina";
    string response = check httpEndpoint->post("/greeting", payload);
    test:assertEquals(response, string `Hello, ${payload}!`);
}
