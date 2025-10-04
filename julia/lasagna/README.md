# Lasagna

Welcome to Lasagna on Exercism's Julia Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

The entire Julia track will require you to treat your solution like small libraries, i.e. you need to define functions, types etc. which will then be run against a test suite.
For that reason, we will introduce named functions as the very first concept.

Julia is a dynamic, strongly-typed programming langauge.
The programming style is mainly functional, though with more flexibility than in languages such as Haskell.

## Variables and assignment

There is no need to declare a variable in advance.
Just assign a value to a suitable name:

```julia-repl
julia> myvar = 42  # an integer
42

julia> name = "Maria"  # strings are surrounded by double-quotes ""
"Maria"
```

## Constants

If a value needs to be available throughout the program, but is not expected to change, it is best to mark it as a constant.

Prefacing an assignment with the `const` keyword allows the compiler to generate more efficient code than is possible for a variable.

Constants also help to protect you against errors in coding.
Accidentally trying to change the `const` value will give a warning:

```julia-repl
julia> const answer = 42
42

julia> answer = 24
WARNING: redefinition of constant Main.answer. This may fail, cause incorrect answers, or produce other errors.
24
```

Note that a `const` can only be declared *outside* any function.
This will typically be near the top of the `*.jl` file, before the function definitions.

## Arithmetic operators

These are the same as in many other languages:

```julia
2 + 3  # 5 (addition)
2 - 3  # -1 (subtraction)
2 * 3  # 6 (multiplication)
8 / 2  # 4.0 (division with floating-point result)
8 % 3  # 2 (remainder)
```

## Functions

There are two common ways to define a named function in Julia:

1. Using the `function` keyword

    ```julia
    function muladd(x, y, z)
        x * y + z
    end
    ```

    Indentation by 4 spaces is conventional for readability, but the compiler ignores this.
    The `end` keyword is essential.

    Note that we could have written `return x * y + z`.
    However, Julia functions always return the last expression evaluated, so the `return` keyword is optional.
    Many programmers prefer to include it to make their intentions more explicit.

2. Using the "assignment form"

    ```julia
    muladd(x, y, z) = x * y + z
    ```

    This is most commonly used for making concise single-expression functions.

    A `return` keyword is *never* used in the assignment form.

The two forms are equivalent, and are used in exactly the same way, so choose whichever is more readable.

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters:

```julia
# invoking a function
muladd(10, 5, 1)

# and of course you can invoke a function within the body of another function:
square_plus_one(x) = muladd(x, x, 1)
```

## Naming conventions

Like many languages, Julia requires that names (of variables, functions, and many other things) start with a letter, followed by any combination of letters, digits and underscores.

By convention, variable, constant, and function names are *lowercase*, with underscores kept to a reasonable minimum.

## Instructions

In this exercise you're going to write some code to help you cook a brilliant lasagna from your favorite cooking book.

You have four tasks, all related to the time spent cooking the lasagna.

## 1. Store the expected bake time in a constant

Define the `expected_bake_time` constant that returns how many minutes the lasagna should bake in the oven.

According to the cooking book, lasagna needs to be in the oven for a total of 60 minutes.

```julia-repl
julia> expected_bake_time
60
```

## 2. Calculate the preparation time in minutes

Define the `preparation_time` function that takes the number of layers you added to the lasagna as an argument and returns how many minutes you spent preparing the lasagna, assuming each layer takes you 2 minutes to prepare.

```julia-repl
julia> preparation_time(4)
8
```

## 3. Calculate the remaining oven time in minutes

Define the `remaining_time` function that takes the actual minutes the lasagna has been in the oven as a parameter and returns how many minutes the lasagna still has to remain in the oven.

```julia-repl
julia> remaining_time(50)
10
```

## 4. Calculate the total working time in minutes

Define the `total_working_time` function that takes two arguments: the first argument is the number of layers you added to the lasagna, and the second argument is the number of minutes the lasagna has been in the oven.

The function should return how many minutes in total you've worked on cooking the lasagna, which is the sum of the preparation time in minutes, and the time in minutes the lasagna has spent in the oven at the moment.

```julia-repl
julia> total_working_time(3, 20)
26
```

## Source

### Created by

- @SaschaMann

### Contributed to by

- @ErikSchierboom
- @glennj
- @colinleach