"""Functions to automate Conda airlines ticketing system."""


def generate_seat_letters(number):
    """Generate a series of letters for airline seats.

    :param number: int - total number of seat letters to be generated.
    :return: generator - generator that yields seat letters.

    Seat letters are generated from A to D.
    After D it should start again with A.

    Example: A, B, C, D

    """

    seat_letters = ['A', 'B', 'C', 'D']
    index = 0

    for i in range(number):
        yield seat_letters[index]
        index = (index + 1) % len(seat_letters)


def generate_seats(number):
    """Generate a series of identifiers for airline seats.

    :param number: int - total number of seats to be generated.
    :return: generator - generator that yields seat numbers.

    A seat number consists of the row number and the seat letter.

    There is no row 13.
    Each row has 4 seats.

    Seats should be sorted from low to high.

    Example: 3C, 3D, 4A, 4B

    """

    seats_per_row = 4
    letter_generator = generate_seat_letters(number)
    row = 0

    for i in range(number):
        if i % seats_per_row == 0:
            row = row + 1
        if row == 13:
            row = row + 1
        yield f'{row}{next(letter_generator)}'


def assign_seats(passengers):
    """Assign seats to passengers.

    :param passengers: list[str] - a list of strings containing names of passengers.
    :return: dict - with the names of the passengers as keys and seat numbers as values.

    Example output: {"Adele": "1A", "Björk": "1B"}

    """

    seat_generator = generate_seats(len(passengers))

    return {p: next(seat_generator) for p in passengers}


def generate_codes(seat_numbers, flight_id):
    """Generate codes for a ticket.

    :param seat_numbers: list[str] - list of seat numbers.
    :param flight_id: str - string containing the flight identifier.
    :return: generator - generator that yields 12 character long ticket codes.

    """

    code_len = 12

    for seat in seat_numbers:
        yield ('%-*s' % (code_len, seat + flight_id)).replace(' ', '0')

