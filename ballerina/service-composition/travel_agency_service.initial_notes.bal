// Client endpoint to communicate with Airline reservation service
// Client endpoint to communicate with Hotel reservation service
// Client endpoint to communicate with Car rental service

// Travel agency service to arrange a complete tour for a user

// Define a resource method to arrange a tour, that accepts `POST` requests in the path `/arrangeTour`.
// This resource should accept a value of the type `TourArrangement` that already defined below.

// Extract Travel infomation from the travel reservation request

// Create the payload skeleton to be sent to the Airline service
// Enrich the required fields with the information retrieved from the original travel reservation request.
// Airline Reservation request shold be in this format : {"name":"", "arrivalDate":"", "departureDate":"", "preference":""}

// If the airline reservation fails, send the response to the client with the follwing payload:
// {"message": "Failed to reserve airline! Provide a valid 'preference' for 'airline' and try again"}
// In case of a failure, status code of the response should be 400 Bad Request.

// Follow the same steps for 'Hotel' and 'Car Rental' services.
// Both hotel and car rental service requests are in the format of : {"name":"", "arrivalDate":"",
// "departureDate":"", "preference":""}
// If the hotel reservation fails, respond with the following payload:
// {"message": "Failed to reserve hotel! Provide a valid 'preference' for 'accommodation' and try again"}
// If the car rental reservation fails, response with the following payload:
// {"message": "Failed to rent car! Provide a valid 'preference' for 'car' and try again"}

// If all three services response positive status, send a successful message to the user
// with the payload {"Message":"Congratulations! Your journey is ready!!"}
// The status code of the response should be 201 Created

// Define a record type to send requests to the reservation services.

// The response type received from the reservation services

// Possible statuses of the reservation service responses
