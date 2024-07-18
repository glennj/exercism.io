def assert(condition; errmsg): if (condition | not) then errmsg | halt_error end;
def refute(condition; errmsg): assert(condition | not; errmsg);

.phrase
| gsub("[() +.-]"; "")         # remove valid non-digits

# length checks
| assert(length >= 10; "must not be fewer than 10 digits")
| assert(length <= 11; "must not be greater than 11 digits")
| if length == 11 then
    assert(startswith("1"); "11 digits must start with 1")
    | .[1:length]
  end

# content checks
| refute(test("[[:alpha:]]"); "letters not permitted")
| refute(test("[^[:digit:]]"); "punctuations not permitted")
| refute(startswith("0"); "area code cannot start with zero")
| refute(startswith("1"); "area code cannot start with one")
| refute(test("^...0"); "exchange code cannot start with zero")
| refute(test("^...1"); "exchange code cannot start with one")
