# Tim from Marketing

Welcome to Tim from Marketing on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Nullability

In Java, the `null` literal is used to denote the absence of a value.

Primitive data types in java all have a default value and therefore can never be `null`.
By convention, they start with a lowercase letter e.g `int`

Reference types contain the memory address of an object can
have a value of null. These variables usually start with an uppercase e.g `String`

Attempting to assign a primitive variable a value of `null` will result in a compile time error as the variable always holds
a primitive value of the type assigned.

```java
//Throws compile time error stating the required type is int, but null was provided
int number = null;
```

Assigning a reference variable a `null` value will not result in a compile time error as reference variables are nullable.

```java
//No error will occur as the String variable str is nullable
String str = null;
```

## Instructions

In this exercise you'll be writing code to print name badges for factory employees.

## 1. Print a badge for an employee

Employees have an ID, name and department name. Employee badge labels are formatted as follows: `"[id] - name - DEPARTMENT"`.
Implement the `Badge.print()` method to return an employee's badge label:

```java
Badge badge = new Badge();
badge.print(734, "Ernest Johnny Payne", "Strategic Communication");
// => "[734] - Ernest Johnny Payne - STRATEGIC COMMUNICATION"
```

Note that the department should be uppercased on the label.

## 2. Print a badge for a new employee

Due to a quirk in the computer system, new employees occasionally don't yet have an ID when they start working at the factory.
As badges are required, they will receive a temporary badge without the ID prefix. Modify the `Badge.print()` method to support new employees that don't yet have an ID:

```java
Badge badge = new Badge();
Badge.print(null, "Jane Johnson", "Procurement");
// => "Jane Johnson - PROCUREMENT"
```

## 3. Print a badge for the owner

Even the factory's owner has to wear a badge at all times.
However, an owner does not have a department. In this case, the label should print `"OWNER"` instead of the department name.
Modify the `Badge.print()` method to print a label for the owner:

```java
Badge badge = new Badge();
badge.print(254, "Charlotte Hale", null);
// => "[254] - Charlotte Hale - OWNER"
```

Note that it is possible for the owner to also be a new employee:

```java
Badge badge = new Badge();
badge.print(null, "Charlotte Hale", null);
// => "Charlotte Hale - OWNER"
```

## Source

### Created by

- @smcg468