public function isLeapYear(int year) returns boolean {
    var isDivBy = function(int divisor) returns boolean => year % divisor == 0;
    return isDivBy(4) && !isDivBy(100) || isDivBy(400);
}
