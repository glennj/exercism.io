import ballerina/http;

service /brainyquote on new http:Listener(9095) {
    resource function get .() returns string {
        return getDefaultQuote();
    }
}

function getDefaultQuote () returns (string) {
    string[] quotes = [ "Many of life's failures are people who did not realize how close they were to success when they gave up. - Thomas A. Edison\n"];
    return quotes[0];
}
