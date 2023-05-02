import ballerina/http;

// Airline reservation service to reserve airline tickets
service /airline on new http:Listener(9091) {

    // Resource to reserve a ticket
    resource function post reserve(@http:Payload AirTicketReservation reservation) returns ServiceResponse {
        string preference = reservation.preference;
        // Mock logic
        if preference !is Class {
            return {
                status: FAILED
            };
        }
        return {
            status: SUCCESS
        };
    }
}

type AirTicketReservation record {|
    string name;
    string arrivalDate;
    string departureDate;
    string preference;
|};

enum Class {
    ECONOMY = "Economy",
    BUSSINESS = "Business",
    FIRST = "First"
}
