# City Office

Welcome to City Office on Exercism's PHP Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Type Declaration

Type declarations in PHP provide type assertions at run time for function arguments, return values and class properties.
On functions, a `void` return type can be added to indicate that no value is returned from the function.
Declared types also serve as run time assertions that functions are returning a reasonably typed response.

```php
<?php

class Driver
{
    private int $serial_number;

    public function setSerialNumber(int $number): void
    {
        $this->serial_number = $number;
    }

    public function getSerialNumber(): int
    {
        return $this->serial_number; 
    }
}

$driver = new Driver();
$driver->setSerialNumber("Version 1b"); // This will throw a TypeError
```

### Type Unions

If a function argument, return value or class property can be more than one type a type union may be declared.

```php
<?php

class IdentityCard
{
    private int|null $id = null;

    public function assign(int|float $id): void
    {
        $this->id = intval($id);
    }
}

$card = new IdentityCard();
$card->assign(5.0);
```

### Mixed Types

When working with code, we may not be able to specify a type.
Starting in PHP 8.0,an escape-hatch type named `mixed` is provided.
Mixed is equivalent to a type union of `object|resource|array|string|float|int|bool|null`.

## Instructions

You have been working in the city office for a while, and you have developed a set of tools that speed up your day-to-day work, for example with filling out forms.

Now, a new colleague is joining you, and you realized your tools might not be self-explanatory.
There are a lot of weird conventions in your office, like always filling out forms with uppercase letters and avoiding leaving fields empty.

As a first step you decide to add PHP type declarations so that it is easier for your new colleague to hop right in and start using your tools.

## 1. Declare the types for the Address class

Add property type declarations to each of the `Address` class properties declared.
Each class property should be declared as a string.

## 2. Declare the types to fill out the form with blank values

Add a parameter type declaration and a return type declaration to the `blanks` method in the `Form` class.
The method should take in an integer length, and return a string representation of the blank line.

## 3. Declare the type when splitting a value into separate letters

Add a parameter type declaration and a return type declaration to the `letters` method in the `Form` class.
The method should take in a string, representing words, and return an array of letters.

## 4. Declare the type when checking if a value will fit into a form

Add parameter type declarations and a return type declaration to the `checkLength` method in the `Form` class.
The method should take in a string word, and an integer maximum length, and return a true or false value.

## 5. Declare the type when formatting an address in the form

Add a parameter type declaration, making use of the `Address` class updated previously, and a return type declaration to the `formatAddress` method in the `Form` class.
The method should take in an `Address` and return a formatted string.

## Source

### Created by

- @neenjaw