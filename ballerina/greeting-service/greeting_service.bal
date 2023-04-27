import ballerina/http;

service on new http:Listener(9090) {
    resource function post greeting(@http:Payload string greetee) returns string|error {
        return "Hello, " + greetee + "!";
    }
}
