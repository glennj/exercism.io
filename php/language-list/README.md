# Language List

Welcome to Language List on Exercism's PHP Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Arrays

Arrays in PHP are ordered maps.
A map is a data structure that associates a key to a value.
Arrays are versatile and can be treated as a linear array (like in `C` or `Java`), list [vector], hash-table, dictionary, collection, stack, or a queue.

A key is optional, but if specified must either be an `int` or a `string`.
When a key is not provided, PHP defaults to integer keys in increasing order, starting at `0`.

```php
$no_keys = ["my", "first", "array"];
$integer_keys = [0 => "my", 1 => "first", 2 => "array"];

$no_keys == $integer_keys // => equal returns true
$no_keys === $integer_keys // => strictly equal returns true
```

A values are of `mixed` type, and each value in an array is not required to be of the same type.

### Using Arrays

Arrays can be declared as a literal (written in code, as done above) or created and manipulated as functions.
Access, assign, append values using the index operator:

```php
$prime_numbers = [1, 3, 5, 7, 11, 12];

$prime_numbers[5] = 13; // replace 12 with 13

$prime_numbers[] = 17; // array now contains [1, 3, 5, 7, 11, 13, 17]
```

## Variable-Length Arguments

Function arguments can be specified such that it can take any number of arguments:

```php
<?php

function actOnItems(...$items) {
  // $items is an array containing 0 or more values
  // used to call the function.
}

actOnItems(1, 2, 3, 4); // $items => [1, 2, 3, 4]
```

## Instructions

In this exercise you need to implement some functions to manipulate a list of programming languages.

## 1. Define a function to return an empty language list

Define the `language_list` function that takes no arguments and returns an empty list.

```php
<?php

language_list();
// => []
```

## 2. Modify function to create a list from any number of languages

Modify the `language_list` function, so it takes a variadic argument or string languages.
It should return the resulting list with the languages in the list.

```php
<?php

$language_list = language_list();
// => []
$language_list = language_list("Clojure", "PHP");
// => ["Clojure", "PHP"]
$language_list = language_list("PHP", "Haskell", "Java", "C++", "Rust")
// => ["PHP", "Haskell", "Java", "C++", "Rust"]
```

## 3. Define a function to add a language to the list

Define the `add_to_language_list` function that takes 2 arguments, an array of languages, and the new language.

```php
<?php

$language_list = language_list();
// => []
$language_list = add_to_language_list($language_list, "Clojure");
// => ["Clojure"]
```

## 4. Define a function to remove an item from the language list

Define the `prune_language_list` function to remove the first language from the array of languages.

```php
<?php

$language_list = language_list("PHP");
// => ["PHP"]
$language_list = prune_language_list($language_list);
// => []
```

## 5. Define a function to get the first item in the list

Define the `current_language` function that takes 1 argument (a _language list_).
It should return the first language in the list.
Assume the list will always have at least one item.

```php
<?php

$language_list = language_list("PHP", "Prolog");
// => ["PHP", "Prolog"]
$first = current_language($language_list);
// => "PHP"
```

## 6. Define a function to return how many languages are in the list

Define the `language_list_length` function that takes 1 argument (a _language list_).
It should return the number of languages in the list.

```php
<?php

$language_list = $language_list("PHP", "Prolog", "Wren");
// => ["PHP", "Prolog", "Wren"]
language_list_length($language_list);
// => 2
```

## Source

### Created by

- @neenjaw