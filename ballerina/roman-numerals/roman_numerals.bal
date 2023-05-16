# Convert an integer to a Roman number.
#
# + number - the integer to convert
# + return - the Roman number as a string
function roman(int number) returns string {
    // with inspiration from https://exercism.org/tracks/ballerina/exercises/roman-numerals/solutions/matthew02

    var dec2rom = function(int digit, string I, string V, string X) returns string {
        match digit {
            1 => {return I;}
            2 => {return I + I;}
            3 => {return I + I + I;}
            4 => {return I + V;}
            5 => {return V;}
            6 => {return V + I;}
            7 => {return V + I + I;}
            8 => {return V + I + I + I;}
            9 => {return I + X;}
            _ => {return "";}
        }
    };

    string numerals = "IVXLCDM  ";
    int n = number;
    int i = 0;
    string roman = "";

    while n > 0 {
        roman = dec2rom(n % 10, numerals[i], numerals[i+1], numerals[i+2]) + roman;
        i += 2;
        n /= 10;
    }
    return roman;
}

function roman_ii(int number) returns string {
    int n = number;     // function parameters are immutable
    string r = "";
    while n >= 1000 { r +=  "M"; n -= 1000; }
    if    n >=  900 { r += "CM"; n -=  900; }
    if    n >=  500 { r +=  "D"; n -=  500; }
    if    n >=  400 { r += "CD"; n -=  400; }
    while n >=  100 { r +=  "C"; n -=  100; }
    if    n >=   90 { r += "XC"; n -=   90; }
    if    n >=   50 { r +=  "L"; n -=   50; }
    if    n >=   40 { r += "XL"; n -=   40; }
    while n >=   10 { r +=  "X"; n -=   10; }
    if    n >=    9 { r += "IX"; n -=    9; }
    if    n >=    5 { r +=  "V"; n -=    5; }
    if    n >=    4 { r += "IV"; n -=    4; }
    while n >=    1 { r +=  "I"; n -=    1; }
    return r;
}
