# Kindergarten Garden

Welcome to Kindergarten Garden on Exercism's Python Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a diagram, determine which plants each child in the kindergarten class is
responsible for.

The kindergarten class is learning about growing plants.
The teacher thought it would be a good idea to give them actual seeds, plant them in actual dirt, and grow actual plants.

They've chosen to grow grass, clover, radishes, and violets.

To this end, the children have put little cups along the window sills, and
planted one type of plant in each cup, choosing randomly from the available
types of seeds.

```text
[window][window][window]
........................ # each dot represents a cup
........................
```

There are 12 children in the class:

- Alice, Bob, Charlie, David,
- Eve, Fred, Ginny, Harriet,
- Ileana, Joseph, Kincaid, and Larry.

Each child gets 4 cups, two on each row.
Their teacher assigns cups to the children alphabetically by their names.

The following diagram represents Alice's plants:

```text
[window][window][window]
VR......................
RG......................
```

In the first row, nearest the windows, she has a violet and a radish.
In the second row she has a radish and some grass.

Your program will be given the plants from left-to-right starting with the row nearest the windows.
From this, it should be able to determine which plants belong to each student.

For example, if it's told that the garden looks like so:

```text
[window][window][window]
VRCGVVRVCGGCCGVRGCVCGCGV
VRCCCGCRRGVCGCRVVCVGCGCV
```

Then if asked for Alice's plants, it should provide:

- Violets, radishes, violets, radishes

While asking for Bob's plants would yield:

- Clover, grass, clover, clover

## Python Implementation

The tests for this exercise expect your program to be implemented as a Garden `class` in Python.
If you are unfamiliar with classes in Python, [classes][classes in python] from the Python docs is a good place to start.

Your `class` should implement a `method` called plants, which takes a student's name as an argument and returns the `list` of plant names belonging to that student.

## Constructors

Creating the example garden

```
[window][window][window]
VRCGVVRVCGGCCGVRGCVCGCGV
VRCCCGCRRGVCGCRVVCVGCGCV
```

would, in the tests, be represented as `Garden("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV")`.

To make this representation work, your `class` will need to implement an `__init__()` method.
If you're not familiar with `__init__()` or _constructors_, [class and instance objects][class vs instance objects in python] from the Python docs gives a more detailed explanation.


## Default Parameters

In some tests, a `list` of students is passed as an argument to `__init__()`.
This should override the twelve student roster provided in the problem statement.
Both of these statements need to work with your `__init__()` method:

```Python
# Make a garden based on the default 12-student roster.
Garden("VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV") 

# Make a garden based on a 2-student roster.
Garden("VRCC\nVCGG", students=["Valorie", "Raven"]) 
```


One approach is to make the student `list` a [default argument][default argument values]; the Python docs describe `default parameters` in depth while explaining [function definitions][function definitions].


[classes in python]: https://docs.python.org/3/tutorial/classes.html
[class vs instance objects in python]: https://docs.python.org/3/tutorial/classes.html#class-objects
[default argument values]: https://docs.python.org/3/tutorial/controlflow.html#default-argument-values
[function definitions]: https://docs.python.org/3/reference/compound_stmts.html#function-definitions

## Source

### Created by

- @sjakobi

### Contributed to by

- @behrtam
- @cmccandless
- @Dog
- @ikhadykin
- @Mofeywalker
- @N-Parsons
- @pheanex
- @smalley
- @thomasjpfan
- @tqa236
- @yawpitch

### Based on

Exercise by the JumpstartLab team for students at The Turing School of Software and Design. - https://turing.edu