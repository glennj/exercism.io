import ballerina/http;

// Car rental service to rent cars
service /car on new http:Listener(9093) {

    // Resource to rent a car
    resource function post rent(@http:Payload CarReservation reservation) returns ServiceResponse {

        // Mock logic
        // If request is for an available car type, send a rental successful status
        string preferredType = reservation.preference;
        if preferredType.toLowerAscii() == AC || preferredType.toLowerAscii() == NORAML {
            return {
                status: SUCCESS
            };
        }
        return {
            status: FAILED
        };
    }
}

type CarReservation record {|
    string name;
    string arrivalDate;
    string departureDate;
    string preference;
|};

enum CarType {
    AC = "air conditioned",
    NORAML = "normal"
}
