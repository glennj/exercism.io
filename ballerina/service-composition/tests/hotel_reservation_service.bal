import ballerina/http;

// Hotel reservation service to reserve hotel rooms
service /hotel on new http:Listener(9092) {

    // Resource to reserve a room
    resource function post reserve(@http:Payload HotelReservation reservation) returns ServiceResponse {
        string preference = reservation.preference;
        // Mock logic
        if preference !is RoomType {
            return {
                status: FAILED
            };
        }
        return {
            status: SUCCESS
        };
    }
}

type HotelReservation record {|
    string name;
    string arrivalDate;
    string departureDate;
    string preference;
|};

enum RoomType {
    SUPERIOR = "superior",
    DELUX = "delux"
}
