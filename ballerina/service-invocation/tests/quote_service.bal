import ballerina/http;
import ballerina/random;

string[] quotes = [
    "Many of life's failures are people who did not realize how close they were to success when they gave up. - Thomas A. Edison",
    "Believe you can and you're halfway there. — Theodore Roosevelt",
    "Be sure you put your feet in the right place, then stand firm. - Abraham Lincoln",
    "Strive not to be a success, but rather to be of value. — Albert Einstein",
    "A person who never made a mistake never tried anything new.—— Albert Einstein",
    "We can easily forgive a child who is afraid of the dark; the real tragedy of life is when men are afraid of the light. — Plato"
];

service /brainyquote on new http:Listener(9095) {
    resource function get .() returns string {
        return getRandomQuote();
    }
}

function getRandomQuote() returns string {
    int|error index = random:createIntInRange(0, quotes.length());
    if index is error {
        return quotes[0];
    }
    return quotes[index];
}
