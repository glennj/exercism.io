# pacman-rules

Welcome to pacman-rules on Exercism's Lua Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Booleans

The boolean type in Lua has the two traditional boolean values: `true` and `false`. These values can be assigned to a variable, combined with logical operators (`and`, `or`, `not`) and used in conditional tests (e.g. in control structures).

```lua
local is_true = true
local is_false = false

true and true  --> true
true and false --> false
false or true  --> true
false or false --> false
not false      --> true
not true       --> false

-- prints 0
if is_true == true then
  print(0)
else
  print(1)
end
```

Conditional tests and the logical operators consider both the boolean `false` and `nil` as false and anything else as true.
```lua
1 and 2          --> 2
nil and 2        --> nil
false and 2      --> false
1 or 2           --> 1
false or 'hello' --> "hello"
nil or false     --> false
```

Both `and` and `or` uses *short-circuit evaluation*, which means they evaluate their second operand only when necessary. All operations are evaluated according to the [operator precedence](https://www.lua.org/manual/5.4/manual.html#3.4.8), where `not` is evaluated before `and` and `or`.

## Instructions

In this exercise, you need to implement some rules from the classic game Pac-Man.

You have four rules to implement, all related to the game states.

## 1. Define if Pac-Man eats a ghost

Implement a function that takes in if Pac-Man has a power pellet active and if Pac-Man is touching a ghost and returns a boolean value if Pac-Man is able to eat the ghost. The function should return `true` only if Pac-Man has a power pellet active and is touching a ghost.

```lua
rules.eat_ghost(false, true)
-- => false
```

## 2. Define if Pac-Man scores

Implement a function that takes in if Pac-Man is touching a power pellet and if Pac-Man is touching a dot and returns a boolean value if Pac-Man scored. The function should return `true` if Pac-Man is touching a power pellet or a dot.

```lua
rules.score(true, true)
-- => true
```

## 3. Define if Pac-Man loses

Implement a function that takes in if Pac-Man has a power pellet active and if Pac-Man is touching a ghost and returns a boolean value if Pac-Man loses. The function should return `true` if Pac-Man is touching a ghost and does not have a power pellet active.

```lua
rules.lose(false, true)
-- => true
```

## 4. Define if Pac-Man wins

Implement a function that takes in if Pac-Man has eaten all of the dots, if Pac-Man has a power pellet active and if Pac-Man is touching a ghost and returns a boolean value if Pac-Man wins. The function should return `true` if Pac-Man has eaten all of the dots and has not lost based on the arguments defined in part 3.

```lua
rules.win(false, true, false)
-- => false
```

## Source

### Created by

- @imolein