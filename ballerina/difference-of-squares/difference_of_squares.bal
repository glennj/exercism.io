function seq(int 'from = 0, int upto = 0, int 'by = 1) returns int[] {
    return from int i in int:range('from, upto, 'by) select i;
}

public function squareOfSum(int n) returns int {
    var sum = int:sum(...seq(upto = n+1));
    return sum * sum;
}

public function sumOfSquares(int n) returns int {
    return int:sum(...seq(upto = n+1).map(x => x * x));
}

public function differenceOfSquares(int n) returns int {
    return (sumOfSquares(n) - squareOfSum(n)).abs();
}
