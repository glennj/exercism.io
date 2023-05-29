import ballerina/http;

final http:Client airlineReservationEP = check new ("http://localhost:9091/airline");
final http:Client hotelReservationEP = check new ("http://localhost:9092/hotel");
final http:Client carRentalEP = check new ("http://localhost:9093/car");

// Travel agency service to arrange a complete tour for a user
service /travel on new http:Listener(9090) {

    resource function post arrangeTour(TourArrangement tourArrangement) returns http:StatusCodeResponse|error {
        Preference pref = tourArrangement.preference;
        Reservation reservation = {
            name: tourArrangement.name,
            arrivalDate: tourArrangement.arrivalDate,
            departureDate: tourArrangement.departureDate,
            preference: ""
        };

        reservation.preference = pref.airline;
        ServiceResponse response = check airlineReservationEP->/reserve.post(reservation);
        if response.status == FAILED {
            http:BadRequest err = {body: {message: "Failed to reserve airline! Provide a valid 'preference' for 'airline' and try again"}};
            return err;
        }

        reservation.preference = pref.accomodation;
        response = check hotelReservationEP->/reserve.post(reservation);
        if response.status == FAILED {
            http:BadRequest err = {body: {message: "Failed to reserve hotel! Provide a valid 'preference' for 'accommodation' and try again"}};
            return err;
        }

        reservation.preference = pref.car;
        response = check carRentalEP->/rent.post(reservation);
        if response.status == FAILED {
            http:BadRequest err = {body: {message: "Failed to rent car! Provide a valid 'preference' for 'car' and try again"}};
            return err;
        }

        http:Created ok = {body: {message: "Congratulations! Your journey is ready!!"}};
        return ok;
    }
}

# The payload type received from the tour arrangement service.
#
# + name - Name of the tourist
# + arrivalDate - The arrival date of the tourist
# + departureDate - The departure date of the tourist
# + preference - The preferences for the airline, hotel, and the car rental
type TourArrangement record {|
    string name;
    string arrivalDate;
    string departureDate;
    Preference preference;
|};

# The different prefenrences for the tour.
#
# + airline - The preference for airline ticket. Can be `First`, `Bussiness`, `Economy`
# + accomodation - The prefenerece for the hotel reservarion. Can be `delux` or `superior`
# + car - The preference for the car rental. Can be `air conditioned`, or `normal`
type Preference record {|
    string airline;
    string accomodation;
    string car;
|};

// Define a record type to send requests to the reservation services.
type Reservation record {|
    string name;
    string arrivalDate;
    string departureDate;
    string preference;
|};

// The response type received from the reservation services
type ServiceResponse record {|
    Status status;
|};

// Possible statuses of the reservation service responses
enum Status {
    SUCCESS = "Success",
    FAILED = "Failed"
}
