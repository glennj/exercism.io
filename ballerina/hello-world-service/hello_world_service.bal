import ballerina/http;

// Implement a general http service listener on port 9090 with the root path "/"
service / on new http:Listener(9090) {

    // add a GET resource called "greeting" that returns the "Hello, World!" message
    resource function get greeting() returns string {
        return "Hello, World!";
    }
}
