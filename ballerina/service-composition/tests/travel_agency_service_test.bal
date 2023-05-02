import ballerina/test;
import ballerina/http;

// Client endpoint
http:Client clientEP = check new ("http://localhost:9090/travel");

// Function to test Travel agency service
@test:Config
function testTravelAgencyService() returns error? {
    TourArrangement arrangement = {
        name: "Alice",
        arrivalDate: "12-03-2022",
        departureDate: "30-03-2022",
        preference: {
            airline: "First",
            accomodation: "delux",
            car: "Air Conditioned"
        }
    };

    // Send a 'post' request and obtain the response
    record {|string message;|} response = check clientEP->post("/arrangeTour", arrangement);
    string expectedMessage = "Congratulations! Your journey is ready!!";
    test:assertEquals(response.message, expectedMessage, msg = "Response mismatch!");
}

@test:Config
function testInvalidAirlineRequest() returns error? {
    TourArrangement arrangement = {
        name: "Alice",
        arrivalDate: "12-03-2022",
        departureDate: "30-03-2022",
        preference: {
            airline: "Second",
            accomodation: "delux",
            car: "Air Conditioned"
        }
    };
    http:Response response = check clientEP->post("/arrangeTour", arrangement);
    // Expected response code is 400 Bad Request
    test:assertEquals(response.statusCode, 400, msg = "Travel agency service did not respond with 400 Bad Request response!");
    // Check whether the response is as expected
    record {|string message;|} responseBody = check (check response.getJsonPayload()).cloneWithType();
    string expectedStatus = "Failed to reserve airline! Provide a valid 'preference' for 'airline' and try again";
    test:assertEquals(responseBody.message, expectedStatus, msg = "Response mismatch!");
}

@test:Config
function testInvalidHotelReservationRequest() returns error? {
    TourArrangement arrangement = {
        name: "Alice",
        arrivalDate: "12-03-2022",
        departureDate: "30-03-2022",
        preference: {
            airline: "First",
            accomodation: "Suite",
            car: "Air Conditioned"
        }
    };
    http:Response response = check clientEP->post("/arrangeTour", arrangement);
    // Expected response code is 400 Bad Request
    test:assertEquals(response.statusCode, 400, msg = "Travel agency service did not respond with 400 Bad Request response!");
    // Check whether the response is as expected
    record {|string message;|} responseBody = check (check response.getJsonPayload()).cloneWithType();
    string expectedStatus = "Failed to reserve hotel! Provide a valid 'preference' for 'accommodation' and try again";
    test:assertEquals(responseBody.message, expectedStatus, msg = "Response mismatch!");
}

@test:Config
function testInvalidCarRentalRequest() returns error? {
    TourArrangement arrangement = {
        name: "Alice",
        arrivalDate: "12-03-2022",
        departureDate: "30-03-2022",
        preference: {
            airline: "First",
            accomodation: "delux",
            car: "full a/c"
        }
    };
    http:Response response = check clientEP->post("/arrangeTour", arrangement);
    // Expected response code is 400 Bad Request
    test:assertEquals(response.statusCode, 400, msg = "Travel agency service did not respond with 400 Bad Request response!");
    // Check whether the response is as expected
    record {|string message;|} responseBody = check (check response.getJsonPayload()).cloneWithType();
    string expectedStatus = "Failed to rent car! Provide a valid 'preference' for 'car' and try again";
    test:assertEquals(responseBody.message, expectedStatus, msg = "Response mismatch!");
}
