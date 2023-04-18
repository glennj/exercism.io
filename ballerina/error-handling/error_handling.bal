import ballerina/http;
import ballerina/io;

final http:Client brainyQuoteClient = check new ("http://localhost:9095/brainyquote");

final http:Client legacyClient = check new ("http://localhost:6060/legacyquote");

public function main() returns error? {

    string|error result = "";

    result = brainyQuoteClient->get("/");
    if result is error {
        io:println(result.message());
    } else {
        io:println(result);
    }

    result = legacyClient->get("/");
    if result is error {
        io:println(result.message());
    } else {
        io:println(result);
    }

    // `get` is the default method for an http:Client, so we
    // can just provide the path
    string _ = check legacyClient->/;
}
