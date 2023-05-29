import ballerina/http;
import ballerina/io;

final http:Client brainyQuoteClient = check new ("http://localhost:9095/brainyquote");

public function main() {
    string|error response = brainyQuoteClient->/;
    if response is error {
        io:println(response.message());
    } else {
        io:println(response);
    }
}
