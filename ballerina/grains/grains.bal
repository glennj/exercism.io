const int SQUARES = 64;

public function square(int n) returns float|error {
    if n < 1 || n > SQUARES {
        return error("Invalid square");
    }
    return float:pow(2.0, <float>(n - 1));
}

public function total() returns float {
    return float:pow(2.0, <float>SQUARES) - 1.0;
}

// Ballerina has the `int` type which is a signed 64 bit integer.
// It has Signed32 and Unsigned32 types but not Signed64 or Unsigned64
// :(
// Hence, floats
