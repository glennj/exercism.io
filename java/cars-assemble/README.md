# Cars, Assemble!

Welcome to Cars, Assemble! on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Numbers

There are two different types of numbers in Java:

- Integers: numbers with no digits behind the decimal separator (whole numbers).
  Examples are `-6`, `0`, `1`, `25`, `976` and `-500000`.
- Floating-point numbers: numbers with zero or more digits behind the decimal separator.
  Examples are `-20.4`, `0.1`, `2.72`, `16.984025` and `1024.0`.

The two most common numeric types in Java are `int` and `double`.
An `int` is a 32-bit integer and a `double` is a 64-bit floating-point number.

Arithmetic is done using the standard arithmetic operators.
Numbers can be compared using the standard numeric comparison operators (eg. `5 > 4` and `4 <= 5`) and the equality (`==`) and inequality (`!=`) operators.

Java has two types of numeric conversions:

1. Implicit conversions: no data will be lost and no additional syntax is required.
2. Explicit conversions: data could be lost and additional syntax in the form of a _cast_ is required.

As an `int` has less precision than a `double`, converting from an `int` to a `double` is safe and is thus an implicit conversion.
However, converting from a `double` to an `int` could mean losing data, so that requires an explicit conversion.

## If-Else Statements

### The _if-then_ statement

The most basic control flow statement in Java is the _if-then_ statement.
This statement is used to only execute a section of code if a particular condition is `true`.
An _if-then_ statement is defined using the `if` clause:

```java
class Car {
    void drive() {
        // the "if" clause: the car needs to have fuel left to drive
        if (fuel > 0) {
            // the "then" clause: the car drives, consuming fuel
            fuel--;
        }
    }
}
```

In the above example, if the car is out of fuel, calling the `Car.drive` method will do nothing.

### The _if-then-else_ statement

The _if-then-else_ statement provides an alternative path of execution for when the condition in the `if` clause evaluates to `false`.
This alternative path of execution follows an `if` clause and is defined using the `else` clause:

```java
class Car {
    void drive() {
        if (fuel > 0) {
            fuel--;
        } else {
            stop();
        }
    }
}
```

In the above example, if the car is out of fuel, calling the `Car.drive` method will call another method to stop the car.

The _if-then-else_ statement also supports multiple conditions by using the `else if` clause:

```java
class Car {
    void drive() {
        if (fuel > 5) {
            fuel--;
        } else if (fuel > 0) {
            turnOnFuelLight();
            fuel--;
        } else {
            stop();
        }
    }
}
```

In the above example, driving the car when the fuel is less then or equal to `5` will drive the car, but it will turn on the fuel light.
When the fuel reaches `0`, the car will stop driving.

## Instructions

In this exercise you'll be writing code to analyze the production of an assembly line in a car factory. The assembly line's speed can range from `0` (off) to `10` (maximum).

At its lowest speed (`1`), `221` cars are produced each hour. The production increases linearly with the speed. So with the speed set to `4`, it should produce `4 * 221 = 884` cars per hour. However, higher speeds increase the likelihood that faulty cars are produced, which then have to be discarded. The following table shows how speed influences the success rate:

- `1` to `4`: 100% success rate.
- `5` to `8`: 90% success rate.
- `9`: 80% success rate.
- `10`: 77% success rate.

You have two tasks.

## 1. Calculate the production rate per hour

Implement the `CarsAssemble.productionRatePerHour()` method to calculate the assembly line's production rate per hour, taking into account its current assembly line's speed and success rate:

```Java
CarsAssemble.productionRatePerHour(6)
// => 1193.4
```

Note that the value returned is a `double`.

## 2. Calculate the number of working items produced per minute

Implement the `CarsAssemble.workingItemsPerMinute()` method to calculate how many working cars are produced per minute:

```Java
CarsAssemble.workingItemsPerMinute(6)
// => 19
```

Note that the value returned is an `int`.

## Source

### Created by

- @TalesDias