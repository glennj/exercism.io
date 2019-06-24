function isarmstrong(n)
    n >= 0 || error("must be non-negative")
    n == digits(n).^(ndigits(n)) |> sum
end
