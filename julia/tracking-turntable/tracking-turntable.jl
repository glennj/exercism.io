# Implement the `z(x, y)` function which takes an `x` coordinate, a `y`
# coordinate from the complex plane and returns the equivalent complex number.
z = complex

# Implement the `euler(r, θ)` function, which takes a radial coordinate `r`, an
# angle `θ` (in radians) and returns the equivalent complex number
# Recall: cis(θ) == exp(im * θ)
euler(r, θ) = r * cis(θ)

# Implement the `rotate(x, y, θ)` function which takes an `x` coordinate, a `y`
# coordinate and an angular displacement `θ` (in radians).  The function should
# rotate the point about the origin by the given angle `θ` and return the new
# coordinates as a tuple: `(x', y')`.
rotate(x, y, θ) = z(x, y) * cis(θ) |> reim

# Implement the function `rdisplace(x, y, r)` which takes an `x` coordinate, a
# `y` coordinate and a radial displacement `r`.  The function should displace
# the x-y coordinates along the radius by the amount `r` and return the new
# coordinates as a tuple: `(x', y')`.
function rdisplace(x, y, Δr)
    r, θ = polar(x, y)
    euler(r + Δr, θ) |> reim
end

# Implement a function `findsong(x, y, r, θ)` which takes the `x` and `y`
# coordinates of the needle as well as the `r` and `θ` displacement between the
# needle and the beginning of the desired song. The new coordinates of the
# needle should be returned as a tuple: `(x', y')`.
findsong(x, y, r, θ) = rotate(rdisplace(x, y, r)..., θ)


# a helper function, the inverse of `euler`
function polar(x, y)
    n = complex(x, y)
    r = abs(n)
    θ = angle(n)
    (r, θ)
end
