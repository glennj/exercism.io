# Highschool Sweethearts

Welcome to Highschool Sweethearts on Exercism's PHP Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Strings

Strings are a series of characters, surrounded by quotes.
Both single and double quotes are supported.

```php
<?php

$where = "Spain";
$what = 'rain';
```

### String variable expansion

In double quoted strings, if a variable is referenced while it is in scope, the value is inserted into the string.

```php
<?php

$answer = 42;
$expanded = "The answer to life, the universe, and everything is $answer"; 
```

### Character Encoding Support

Historically, string functions in PHP only supported ASCII characters and did not support modern Unicode encodings.
ASCII characters are each 1 byte, whereas Unicode characters may be 1-4 bytes in length.
If needing to manipulate Unicode strings safely, refer to the [multibyte versions][multi-byte-fns] of the historic functions.

```php
<?php

$byte_length = strlen('😃'); // => 4
$string_length = mb_strlen('😃'); // => 1
```

[multi-byte-fns]: https://www.php.net/manual/en/ref.mbstring.php

## Instructions

In this exercise, you are going to help high school sweethearts profess their love on social media by generating an ASCII heart with their initials:

```
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     J. K.  +  M. B.     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *
```

## 1. Get the name's first letter

Implement the `HighSchoolSweetheart::firstLetter` function.
It should take a name and return its first letter.
It should clean up any unnecessary whitespace from the name.

```php
<?php

$sweetheart = new HighSchoolSweetheart();
$sweetheart->firstLetter("Jane");
# => "J"
```

## 2. Format the first letter as an initial

Implement the `HighSchoolSweetheart::initial` function.
It should take a name and return its first letter, uppercase, followed by a dot.
Make sure to reuse `HighSchoolSweetheart::first_letter` that you defined in the previous step.

```php
<?php

$sweetheart = new HighSchoolSweetheart();
$sweetheart->initial("jane");
# => "J."
```

## 3. Split the full name into the first name and the last name

Implement the `HighSchoolSweetheart::initials` function.
It should take a full name, consisting of a first name and a last name separated by a space, and return the initials.
Make sure to reuse `HighSchoolSweetheart::initial` that you defined in the previous step.

```php
<?php

$sweetheart = new HighSchoolSweetheart();
$sweetheart->initials("Jane Doe");
# => "J. D."
```

## 4. Put the initials inside of the heart

Implement the `HighSchoolSweetheart::pair` function.
It should take two full names and return the initials inside an ASCII heart.
Make sure to reuse `HighSchoolSweetheart::initials` that you defined in the previous step.

```php
<?php

$sweetheart = new HighSchoolSweetheart();
$sweetheart->pair("Blake Miller", "Riley Lewis")
# => """
#      ******       ******
#    **      **   **      **
#  **         ** **         **
# **            *            **
# **                         **
# **     B. M.  +  R. L.     **
#  **                       **
#    **                   **
#      **               **
#        **           **
#          **       **
#            **   **
#              ***
#               *
# """
```

## Source

### Created by

- @neenjaw