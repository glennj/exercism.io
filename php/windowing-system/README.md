# Windowing System

Welcome to Windowing System on Exercism's PHP Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Classes

Classes are a unit of code organization in PHP.
Code represents the behavior of an item or the behavior of a process can be grouped together as a class.
Functions that are assigned to a class are called `methods` by convention.

```php
<?php

class Door
{
    function lock()
    {
        // ...
    }

    function unlock()
    {
        // ...
    }
}
```

Classes are instantiated with the `new` keyword, this allocates memory for the class instance and calls the classes constructor method.
Instantiated classes may be assigned to a variable.
To invoke the methods of a class instance, we can use the `->` access operator.

```php
<?php

$a_door = new Door();
$a_door->lock();
$a_door->unlock();
```

An instantiated class may reference its own instance using the special `$this` variable.
Classes may also have properties, these act as variables that are tied to the instance of the class object.
These properties may be referenced internally or externally.

```php
<?php

class Car
{
    public $color

    function __construct($color)
    {
        $this->color = $color;
    }

    function getColor()
    {
        return $this->color;
    }
}

$a_car = new Car("red");
$a_car->color; // => "red" by accessing the property
$a_car->getColor(); // => "red" by invoking the instance method
```

## Instructions

In this exercise, you will be simulating a windowing based computer system.
You will create some windows that can be moved and resized.
The following image is representative of the values you will be working with below.

```text
 ╔════════════════════════════════════════════════════════════╗
 ║                                                            ║
 ║        position::$x,_                                      ║
 ║        position::$y  \                                     ║
 ║                       \<---- size::$width ---->            ║
 ║                 ^      *──────────────────────┐            ║
 ║                 |      │        title         │            ║
 ║                 |      ├──────────────────────┤            ║
 ║                 |      │                      │            ║
 ║          size::$height │                      │            ║
 ║                 |      │       contents       │            ║
 ║                 |      │                      │            ║
 ║                 |      │                      │            ║
 ║                 v      └──────────────────────┘            ║
 ║                                                            ║
 ║                                                            ║
 ╚════════════════════════════════════════════════════════════╝
```

## 1. Define the properties of the Program Window

Using the provided class scaffold, provide the properties for the `x`, `y`, `height`, and `width` values.

```php
<?php

$window = new ProgramWindow();
$window->x; // => null
$window->y; // => null
$window->width; // => null
$window->height; // => null
```

## 2. Define the initial values for the program window

Define a constructor function for `ProgramWindow`.
It should not take any arguments, but during the constructor execution set the default values for its properties.
It should set the initial `x` and `y` values to `0`.
It should set the initial `width` and `height` to an `800x600` screen size.

```php
<?php

$window = new ProgramWindow();
$window->x; // => 0 
$window->y; // => 0
$window->width; // => 800
$window->height; // => 600
```

## 3. Define a function to resize the window

Define a new class in `Size.php` for a Size class.
It should take constructor arguments to set an `height` and `width` property.
Additionally `ProgramWindow` requires a resize function, which receives the `Size` object instance and updates its own properties.

```php
<?php 

$window = new ProgramWindow();
$size = new Size(764, 1080);
$window->resize($size)

$window->height;
// => 764
$window->width;
// => 1080
```

## 4. Define a function to move the window

Define a new class in `Position.php` for a Position class.
It should take constructor arguments to set a `y` and `x` property.
Additionally `ProgramWindow` requires a move function, which receives the `Position` object instance and updates its own properties.

```php
<?php 

$window = new ProgramWindow();
$position = new Position(80, 313);
$window->move($position)

$window->y;
// => 80
$window->x;
// => 313
```

## Source

### Created by

- @neenjaw