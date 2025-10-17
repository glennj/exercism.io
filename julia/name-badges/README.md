# Name Badges

Welcome to Name Badges on Exercism's Julia Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

Many languages have a way such as `null` or `none` to indicate a non-existent value.
Because Julia is designed to handle large volumes of (often messy) data, it has multiple forms of nothingness.

The overall aim is to flag missing or suspect values as they are encountered, then continue without raising an exception.

## `nothing`

If a value really does not exist, it is represented by `nothing`. This is probably closest to what C (`NULL`) or Python (`None`) might do.

```julia-repl
julia> n = nothing

julia> isnothing(n)
true

julia> typeof(n)
Nothing
```

So `nothing` is a singleton value of type `Nothing`, and we can test for it.

One common use of `nothing` is as a return (non-)value for functions which are used only for their side effects (printing, network configuration, or whatever).

## `missing`

For situations where a value exists in theory but we don't know what it is, `missing` is used. 
For example, when counting vehicles traveling on a road, human observers might need a break or automatic sensors break down, but the traffic continues to flow.

Thus `missing` is a placeholder, warning humans that they need to make a decision about how to handle this gap in the data.

```julia-repl
julia> mv = [1, 2, missing]
3-element Vector{Union{Missing, Int64}}:
 1
 2
  missing

julia> typeof(mv)
Vector{Union{Missing, Int64}} (alias for Array{Union{Missing, Int64}, 1})

julia> ismissing.(mv)  # broadcast function, displays as 1 for true, 0 for false
3-element BitVector:
 0
 0
 1
```

Few other languages have this feature built in, but close analogues are `NA` in R or `NULL` in SQL.

Expressions usually return `missing` by default if `missing` values are present.
If you want these values to be ignored, use the `skipmissing()` function to make this explicit:

```julia-repl
julia> mv = [1, 2, missing]
3-element Vector{Union{Missing, Int64}}:
 1
 2
  missing

julia> sum(mv)  # missing in, missing out
missing

julia> skipmissing(mv)
skipmissing(Union{Missing, Int64}[1, 2, missing])

julia> collect(skipmissing(mv))
2-element Vector{Int64}:
 1
 2

julia> sum(skipmissing(mv))  # functions like sum() can work with iterators
3
```

Because `skipmissing` creates an iterator, wrap it in `collect()` if you need a vector.

Sometimes it is useful to replace `missing` values with some default.
The `@coalesce()` macro is useful for this, as it will return the first non-missing value (or `missing` if there is nothing else).

```julia-repl
julia> str = ["I", "exist", missing]
3-element Vector{Union{Missing, String}}:
 "I"
 "exist"
 missing

julia> [@coalesce(s, "-") for  s in str]
3-element Vector{String}:
 "I"
 "exist"
 "-"
```

## `NaN`

Short for "Not a Number", NaN flags a computation problem in situations where a number was expected.

```julia-repl
julia> v = [0, 1, -1]
3-element Vector{Int64}:
  0
  1
 -1

julia> v / 0
3-element Vector{Float64}:
 NaN
  Inf
 -Inf
 
julia> sum(v / 0)
NaN
```

Any sort of calculation on data including a NaN will give a `NaN` result.

There is currently no special function to remove `NaN` values, but the standard `filter()` function can do this quite simply.
Only values for which some given condition is `true` will be copied to the result array:

```julia-repl
julia> filter(!isnan, [1, 2, NaN])
2-element Vector{Float64}:
 1.0
 2.0
```

## Instructions

In this exercise you'll be writing code to create name badges for factory employees to wear. Employees have an ID, name, and department name. Employee badge labels are formatted as follows: `"[id] - name - DEPARTMENT"`.

## 1. Create a badge for an employee

Implement the `print_name_badge` function. It should take an ID, name, and a department. It should return the badge label, with the department name in uppercase.

```julia-repl
julia> print_name_badge(67, "Katherine Williams", "Strategic Communication")
"[67] - Katherine Williams - STRATEGIC COMMUNICATION"
```

## 2. Create a badge for a new employee

Due to a quirk in the computer system, new employees occasionally don't yet have an ID when they start working at the factory. As badges are required, they will receive a temporary badge without the ID prefix.

Extend the `print_name_badge` function. When the id is missing, it should create a badge without it.

```julia-repl
julia> print_name_badge(missing, "Robert Johnson", "Procurement")
"Robert Johnson - PROCUREMENT"
```

## 3. Create a badge for the owner

Even the factory's owner has to wear a badge at all times. However, an owner does not have a department and never will: they are above all the departments. In this case, the label should return `"OWNER"` instead of the department name.

Extend the `print_name_badge` function. When the department is `nothing`, assume the badge belongs to the company owner.

```julia-repl
julia> print_name_badge(204, "Rachel Miller", nothing)
"[204] - Rachel Miller - OWNER"
```

Note that it is possible for the owner to also be a new employee.

```julia-repl
julia> print_name_badge(missing, "Rachel Miller", nothing)
"Rachel Miller - OWNER"
```

## 4. Calculate the total salary of emplyees with no ID

As a rough metric of how well the IDs are being issued, you want to see the combined salary of employees with no ID. A high value means lots are waiting, or the problem is affecting senior people: both bad.

Implement the `salaries_no_id` function that takes a vector of IDs and a corresponding vector of salaries, and returns the sum of salaries for people with no ID yet. Both vectors are the same length.

```julia-repl
julia> ids = [204, missing, 210, 352, missing, 263]
julia> salaries = [23, 21, 47, 35, 17, 101] * 1000
julia> salaries_no_id(ids, salaries)
38,000
```

## Source

### Created by

- @colinleach