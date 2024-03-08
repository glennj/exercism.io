function ident(x)  {return x}
function square(x) {return x*x}

function square_of_sum(n)  {return calculator(n, "ident", "square")}
function sum_of_squares(n) {return calculator(n, "square", "ident")}
function difference(n)     {return square_of_sum(n) - sum_of_squares(n)}

function calculator(n, inner_fn, outer_fn,   i, sum) {
    sum = 0
    for (i = 1; i <= n; i++) {
        sum += @inner_fn(i)
    }
    return @outer_fn(sum)
}

BEGIN {FS = ","}
{
    f = $1
    print @f($2)
}
