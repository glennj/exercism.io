public function collatzSteps(int n) returns int|error {
    if n < 1 {
        return error("Only positive integers are allowed");
    }
    return collatzHelper(n);
}

function collatzHelper(int n, int steps = 0) returns int {
    match n {
        1               => { return steps; }
        _ if n % 2 == 0 => { return collatzHelper(n / 2, 1 + steps); }
        _               => { return collatzHelper(3*n + 1, 1 + steps); }
    }
}
