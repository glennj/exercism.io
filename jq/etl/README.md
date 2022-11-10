# ETL

Welcome to ETL on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

We are going to do the `Transform` step of an Extract-Transform-Load.

## ETL

Extract-Transform-Load (ETL) is a fancy way of saying, "We have some crufty, legacy data over in this system, and now we need it in this shiny new system over here, so we're going to migrate this."

(Typically, this is followed by, "We're only going to need to run this once."
That's then typically followed by much forehead slapping and moaning about how stupid we could possibly be.)

## The goal

We're going to extract some Scrabble scores from a legacy system.

The old system stored a list of letters per score:

- 1 point: "A", "E", "I", "O", "U", "L", "N", "R", "S", "T",
- 2 points: "D", "G",
- 3 points: "B", "C", "M", "P",
- 4 points: "F", "H", "V", "W", "Y",
- 5 points: "K",
- 8 points: "J", "X",
- 10 points: "Q", "Z",

The shiny new Scrabble system instead stores the score per letter, which makes it much faster and easier to calculate the score for a word.
It also stores the letters in lower-case regardless of the case of the input letters:

- "a" is worth 1 point.
- "b" is worth 3 points.
- "c" is worth 3 points.
- "d" is worth 2 points.
- Etc.

Your mission, should you choose to accept it, is to transform the legacy data format to the shiny new format.

## Notes

A final note about scoring, Scrabble is played around the world in a variety of languages, each with its own unique scoring table.
For example, an "E" is scored at 2 in the Māori-language version of the game while being scored at 4 in the Hawaiian-language version.

## `jq` Tips

This exercise is about taking an input object and transforming it into a new object.
A couple of builtin functions that may help:

- [`to_entries`][x_entries]
  - this function takes an object as input
  - it outputs an _array_ -- each array element is an object `{"key": "K", "value": V}`
    - each `K` is a key from the input object
    - each `V` is `K`'s corresponding value.
- [`from_entries`][x_entries]
  - the inverse operation
  - takes an array of `{"key": K, "value": V}` objects and constructs an object

An example:

```sh
jq -n '
    {
        "username": "glennj",
        "tracks": ["bash", "awk", "jq"],
        "reputation": 123
    }
    | to_entries
'
```

outputs

```json
[
  {
    "key": "username",
    "value": "glennj"
  },
  {
    "key": "tracks",
    "value": ["bash", "awk", "jq"]
  },
  {
    "key": "reputation",
    "value": 123
  }
]
```

[x_entries]: https://stedolan.github.io/jq/manual/v1.6/#to_entries,from_entries,with_entries

## Source

### Created by

- @glennj

### Based on

Exercise by the JumpstartLab team for students at The Turing School of Software and Design. - https://turing.edu