
# Find the largest product of the digits of a substring
#
# + digitString - the sequence of digits as a string
# + span - the substring size
# + return - the maximum product, or an error
public function largestProduct(string digitString, int span) returns int|error {
    if span < 0 {
        return error("span must not be negative");
    }
    if span > digitString.length() {
        return error("span must be smaller than string length");
    }

    int zero = string:toCodePointInt("0");
    int[] digits = digitString.toCodePointInts().map(cp => cp - zero);

    if !digits.every(d => 0 <= d && d <= 9) {
        return error("digits input must only contain digits");
    }

    var multiply = function(int a, int b) returns int => a * b;

    int largest = int:MIN_VALUE;
    foreach int offset in 0 ..< (digits.length() - span + 1) {
        int product = digits.slice(offset, offset + span).reduce(multiply, 1);
        largest = int:max(largest, product);
    }
    return largest;
}
