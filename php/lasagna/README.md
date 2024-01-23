# Lucian's Luscious Lasagna

Welcome to Lucian's Luscious Lasagna on Exercism's PHP Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Basics

### Values and Variables

PHP is a general-purpose dynamically-typed programming language.
Value types are only checked while the program is running.
Using the assignment `=` operator, any value type may be assigned to any variable name.
Variable names must start with a dollar `$` sign.

```php
<?php
$count = 1 // Assigned an integer value of 1
$count = 2 // Re-assigned a new value to the variable

$count = false // You may assign any value type

// Strings can be created by enclosing the characters
// within single `'` quotes or double `"` quotes.
$message = "Success!"
```

### Code organization

PHP code files are text files that start with `<?php`.
If the file contains only php code, there is no closing `?>` tag.
Code, functions, and classes may all be present in a single file, but usually classes are in their own file with a filename which matches the class.

```php
<?php
// index.php

$sum = add(1, 2);
```

```php
<?php
// Calculator.php

class Calculator {
    // ...

    function add($x, $y)
    {
        return $x + $y;
    }
}
```

### Naming conventions

Classnames should all be `PascalCase`.
Depending on the style standard; variables, functions, method names may be either `camelCase` or `snake_case`.
Names may contain letters `a-zA-Z`, numbers `0-9`, and underscores `_` but they cannot start with a number.

### Comments

Single line comments start with `//`, and a block of text can be wrapped with `/*` and  `*/` to become a comment.

```php
<?php

// Single line comment

/*
  Multi-line comment
*/
```

## Instructions

// Instructions

In this exercise you're going to write some code to help you cook a brilliant lasagna from your favorite cooking book.

You have five tasks, all related to the time spent cooking the lasagna.

//# 1. Define the expected oven time in minutes

Define the `Lasagna::expectedCookTime` function that does not take any arguments and returns how many minutes the lasagna should be in the oven. According to the cooking book, the expected oven time in minutes is 40:

```php
<?php
$timer = new Lasagna();
$timer->expectedCookTime()
// => 40
```

//# 2. Calculate the remaining oven time in minutes

Define the `Lasagna::remainingCookTime` function that takes the actual minutes the lasagna has been in the oven as an argument and returns how many minutes the lasagna still has to remain in the oven, based on the expected oven time in minutes from the previous task.

```php
<?php
$timer = new Lasagna();
$timer->remainingCookTime(30)
// => 10
```

//# 3. Calculate the preparation time in minutes

Define the `Lasagna::totalPreparationTime` function that takes the number of layers you added to the lasagna as an argument and returns how many minutes you spent preparing the lasagna, assuming each layer takes you 2 minutes to prepare.

```php
<?php
$timer = new Lasagna();
$timer->totalPreparationTime(2)
// => 4
```

//# 4. Calculate the total working time in minutes

Define the `Lasagna::totalElapsedTime` function that takes two arguments: the first argument is the number of layers you added to the lasagna, and the second argument is the number of minutes the lasagna has been in the oven. The function should return how many minutes in total you've worked on cooking the lasagna, which is the sum of the preparation time in minutes, and the time in minutes the lasagna has spent in the oven at the moment.

```php
<?php
$timer = new Lasagna();
$timer->totalElapsedTime(3, 20)
// => 26
```

//# 5. Create a notification that the lasagna is ready

Define the `Lasagna::alarm` function that does not take any arguments and returns a message indicating that the lasagna is ready to eat.

```php
<?php
$timer = new Lasagna();
$timer->alarm()
// => "Ding!"
```

## Source

### Created by

- @neenjaw