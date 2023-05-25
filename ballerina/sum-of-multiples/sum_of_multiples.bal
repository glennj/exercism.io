# Find the unique multiples of the given factors that are less than the limit.
# Return the sum of the multiples.
#
# + factors - an array of integers
# + 'limit - the upper limit of the multiples
# + return - the sum of the multiples
public function sum(int[] factors, int 'limit) returns int {
    // finding all the multiples is straightforward
    int[] multiples = 
        from int f in factors
        where f > 0
        from int m in int:range(f, 'limit, f)
        select m;

    // but getting the *unique* set of multiples, not so much.
    return int:sum(... uniq(multiples));
}

function uniq(int[] ary) returns int[] {
    int[] unq = [];
    foreach int n in ary {
        if unq.indexOf(n) is () {
            unq.push(n);
        }
    }
    return unq;
}
