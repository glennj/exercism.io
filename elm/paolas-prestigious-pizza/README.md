# Paola's Prestigious Pizza

Welcome to Paola's Prestigious Pizza on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Parsing

Parsing is the process of transforming text into meaningful data.
Functions from the `String` package are limiting in that regard and regular expressions are difficult to use, so the idiomatic way in Elm to parse is with the `elm/parser` package.

Let's walk through an example of writing a parser for (a subset of) the programming language [LOLCODE][lolcode].
Let's consider the following commented program:

```
HAI 1                 # program version 1
CAN HAS STDIO?        # import stdio
VISIBLE "HAI WORLD!"  # print "HAI WORLD!"
KTHXBYE               # end of program
```

First, let's write an Elm type to represent the possible commands:

```elm
type Command
    = Version Int
    | Import String
    | Print String
```

Our LOLCODE program can then be modeled as a `List Command`.
Let's start with a parser for the `HAI 1` line:

```elm
versionP : Parser Command
versionP =
    Parser.succeed Version
        |. Parser.keyword "HAI"
        |. Parser.spaces
        |= Parser.int
```

`versionP` is built using a _parser pipeline_.
The `(|.)` operator means "parse the string but throw away the result" and `(|=)` means "parse the string and keep the result".
In this example, `Parser.keyword "HAI"` will consume a "HAI", but that result is ignored.
`Parser.spaces` will consume any number of  ' ', '\n', and '\r' characters, but that will also be ignored.
`Parser.int` will parse an integer, and the value of that integer (say 1) will be passed to the topmost `Parser.succeed Version` and the parser will succeed in returning the value `Version 1`.

If any one of the parser in the pipeline fails, it will return a `Parser.DeadEnd` and the whole parser will fail.
A parser can be ran with `Parser.run`:

```elm
Parser.run versionP "HAI 1"
    --> Ok (Version 1)
Parser.run versionP "HAI 2"
    --> Ok (Version 2)
Parser.run versionP "BYE 2"
    --> Err [{ problem = ExpectingKeyword "HAI", col = 1, row = 1 }]
Parser.run versionP "HAI MOM"
    --> Err [{ problem = ExpectingInt, col = 5, row = 1 }]
```

Let's attack the "CAN HAS STDIO?" line with

```elm
importP : Parser Command
importP =
    Parser.succeed Import
        |. Parser.keyword "CAN HAS"
        |. Parser.spaces
        |= packageP
        |. Parser.symbol "?"
```

`importP` is also a parser pipeline, but this time ends with `Parser.symbol` that is suitable for parsing symbols such as "?".
The value that we want to keep (the name of the package to import) is parsed by

```elm
packageP : Parser String
packageP =
    Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.map String.toLower
```

Since we don't know what the package name will be, we cannot use `Parser.keyword`, so instead we use `Parser.chompWhile` that will consume single characters as long as they satisfy a condition (here, being an alphanumeric character).
These chomped characters are then collected by `Parser.getChompedString` that returns a `Parser String`.
In this case, we would like to normalize the package names, so we use `Parser.map` to modify the string contained in the parser and make it lowercase.

```elm
Parser.run importP "CAN HAS STDIO?"
    --> Ok (Import "stdio")
Parser.run importP "CAN HAS StDiO?"
    --> Ok (Import "stdio")
Parser.run importP "CAN HAS STDIO"
    -->  Err [{ problem = ExpectingSymbol "?", col = 14, row = 1 }]
```

Next up, `VISIBLE "HAI WORLD!"` is parsed by

```elm
printP : Parser Command
printP =
    Parser.succeed Print
        |. Parser.keyword "VISIBLE"
        |. Parser.spaces
        |. Parser.symbol "\""
        |= stringP
        |. Parser.symbol "\""

stringP : Parser String
stringP =
    Parser.chompWhile (\c -> c /= '"')
        |> Parser.getChompedString

Parser.run printP "VISIBLE \"HAI WORLD!\""
    --> Ok (Print "HAI WORLD!")
```

We now have a parser for each command.
Of course, in a general program, the order of these commands cannot be predicted, so we build a generic command parser

```elm
commandP : Parser Command
commandP =
    Parser.oneOf
        [ versionP
        , importP
        , printP
        ]
```

`Parser.oneOf` will try one parser, in the given order.
If a parser fails, the next one is tried, but if it succeeds, its result is returned without trying the remaining parsers.
If all parsers fail, `Parser.oneOf` fails.

```elm
Parser.run commandP "HAI 2"
    --> Ok (Version 2)
Parser.run commandP "CAN HAS STDIO?"
    --> Ok (Import "stdio")
Parser.run commandP "VISIBLE \"HAI WORLD!\""
    --> Ok (Print "HAI WORLD!")
Parser.run commandP "O NOES"
    --> Err [ { problem = ExpectingKeyword "HAI", col = 1, row = 1 }
    --      , { problem = ExpectingKeyword "CAN HAS", col = 1, row = 1 }
    --      , { problem = ExpectingKeyword "VISIBLE", col = 1, row = 1 }
    --      ]
```

Note how each failed attempt produces a `Parser.DeadEnd` that is collected by `Parser.oneOf`.

To parse a full program, we need to parse several commands sequentially with

```elm
programP : Parser (List Command)
programP =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = "KTHXBYE"
        , spaces = Parser.succeed ()
        , item = commandP
        , trailing = Parser.Optional
        }
```

`Parser.sequence` is a very customizable parser with a number of options.
`start`, `separator` and `end` take strings that separate the items of the sequence, for parsing an Elm list we would use "[", "," and "]".
`spaces` provides flexibility by taking a parser that will handle potential spaces between items and separators, for parsing an Elm list we would use `Parser.spaces`, but in this case we don't accept any spaces so we use `Parser.succeed ()` that succeeds immediately without actually consuming anything.
`item` takes the item parser, and finally, `trailing` lets us decide if trailing separators are `Parser.Forbidden`, `Parser.Mandatory` or `Parser.Optional`.

```elm
Parser.run programP "HAI 2\nCAN HAS STDIO?\nKTHXBYE"
    --> Ok [Version 2, Import "stdio"]
Parser.run programP "KTHXBYE"
    --> Ok []
Parser.run programP "HAI 2\nCAN HAS STDIO?\n"
    --> Err [ { problem = ExpectingKeyword "HAI", col = 1, row = 3 }
    --      , { problem = ExpectingKeyword "CAN HAS", col = 1, row = 3 }
    --      , { problem = ExpectingKeyword "VISIBLE", col = 1, row = 3 }
    --      , { problem = Expecting "KTHXBYE", col = 1, row = 3 }
    --      ]
```

When parsers succeed, they stop consuming the string

```elm
Parser.run Parser.int "1"
    --> Ok 1
Parser.run Parser.int "1 BTW VISIBLE \"U SEE NOTHING\""
    --> Ok 1
```

So if we need to make sure we parsed everything in the string, we may use `Parser.end`

```elm
fullProgramP : Parser (List Command)
fullProgramP =
    programP
        |. Parser.spaces
        |. Parser.end

Parser.run fullProgramP "HAI 2\nCAN HAS STDIO?\nKTHXBYE"
    --> Ok [Version 2, Import "stdio"]
Parser.run fullProgramP "HAI 2\nCAN HAS STDIO?\nKTHXBYE\nBTW VISIBLE \"U SEE NOTHING\""
    --> Err [{ problem = ExpectingEnd, col = 1, row = 4 }]
```

Finally, we sometimes need to make a decision to fail a parser depending on its parsed content.
For example, if we needed a  parser that failed programs that start with two "HAI" commands in a row, we could use

```elm
singleVersionProgramP : Parser (List Command)
singleVersionProgramP =
    fullProgramP
        |> Parser.andThen
            (\commands ->
                case commands of
                    (Version _) :: (Version _) :: _ ->
                        Parser.problem "Multiple versions defined"

                    _ ->
                        Parser.succeed commands
            )
```

`Parser.andThen` can inspect the content of a parser and returns a new parser depending on its value.
Here, we failed the parser with a custom message using `Parser.problem` in one case, and returned the original value in the other.

```elm
Parser.run singleVersionProgramP "HAI 2\nCAN HAS STDIO?\nKTHXBYE"
    --> Ok [Version 2, Import "stdio"]
Parser.run singleVersionProgramP "HAI 1\nHAI 2\nCAN HAS STDIO?\nKTHXBYE"
    --> Err [{ problem = Problem "Multiple versions defined", col = 8, row = 4 }]
```

[lolcode]: https://en.wikipedia.org/wiki/LOLCODE

## Instructions

You are excited to join Paola's Prestigious Pizza as the new restaurant manager.
You have many plans about how to run the place efficiently, and in order to do so, you need some data.
First things first, you need to analyze the menu.

There are many types of pizza on the menu, Paola gave you a list in a text file, with one pizza per line.
Each pizza entry has three components: its name, an optional vegetarian indicator, and a price.

## 1. Parse pizza price

You look at the first couple of lines on the menu:

```
Regina: tomato, ham, mushrooms, cantal - 11€
Formaggio (v): tomato, emmental - 8€
...
```

The price seems like a good place to start.
All prices are whole numbers in the Euro currency.

Implement `priceParser` to parse the prices. 
Since all prices are in Euro, you don't need to keep track of the currency, although you still need to parse it.

```elm
Parser.run priceParser "8€"
    --> Ok 8
Parser.run priceParser "8"
    --> Err [{ problem = ExpectingSymbol "€", col = 2, row = 1 }]
```

## 2. Parse vegetarian indicator

You look at the menu again:

```
Regina: tomato, ham, mushrooms, cantal - 11€
Formaggio (v): tomato, emmental - 8€
...
```

The vegetarian indicator `"(v)"` is pretty simple too, you decide to do that next!

Implement `vegetarianParser` to parse the indicator.
The parser should return a `Bool`, `True` if the indicator is there and `False` otherwise. 

```elm
Parser.run vegetarianParser "(v)"
    --> Ok True
Parser.run vegetarianParser ""
    --> Ok False
```

## 3. Parse pizza and ingredient names

Alright, what's next?

```
Regina: tomato, ham, mushrooms, cantal - 11€
Formaggio (v): tomato, emmental - 8€
...
```

The ingredients and pizza names seem fairly straightforward: they are all single words made out of upper case and lower case ASCII characters.

Implement `wordParser` to parse the names.
While you are at it, make all the words lowercase for uniformity.

```elm
Parser.run wordParser "REGINA"
    --> Ok "regina"
Parser.run wordParser "tomato"
    --> Ok "tomato"
Parser.run wordParser "(v)"
    --> Ok ""
Parser.run wordParser ""
    --> Ok ""
```

## 4. Parse a list of ingredients

Ingredients, however, come in group:

```
Regina: tomato, ham, mushrooms, cantal - 11€
Formaggio (v): tomato, emmental - 8€
...
```
They are single words separated by commas and possibly spaces.

Implement `ingredientsParser` to parse the ingredients. 

```elm
Parser.run ingredientsParser "tomato, emmental"
    --> Ok ["tomato", "emmental"]
Parser.run ingredientsParser "tomato"
    --> Ok ["tomato"]
```

## 5. Parse full pizza

Now that you have all the ingredients, time to make some `Pizza`!

```
Regina: tomato, ham, mushrooms, cantal - 11€
Formaggio (v): tomato, emmental - 8€
...
```

First you have the pizza name, the optional vegetarian indicator, a colon `':'`, the ingredient list, a dash `'-'` and the price, with possible spaces interspersed.

Implement `pizzaParser` to parse the full pizza.

```elm
Parser.run pizzaParser "Regina: tomato, ham, mushrooms, cantal - 11€"
    --> Ok (Pizza "regina" False ["tomato", "ham", "mushrooms", "cantal"] 11)
```

## 6. Parse full menu

Party time!

Implement `menuParser` to parse a list of all pizzas separated by newline characters `'\n'` in the text file.
Make sure that you got all the pizzas by running the parser until it reaches the end of the file.

```elm
Parser.run menuParser "Regina: tomato, ham, mushrooms, cantal - 11€\nFormaggio (v): tomato, emmental - 8€"
    --> Ok [Pizza "regina" False ["tomato", "ham", "mushrooms", "cantal"] 11,
    --      Pizza "formaggio" True ["tomato", "emmental"] 8]
Parser.run menuParser "Regina: tomato, ham, mushrooms, cantal - 11€\[END]"
    --> Err [{ problem = ExpectingEnd, col = 1, row = 2 }]
```

## 7. Parse multi-word ingredient names

Oh no, it seems you missed something.
Somewhere further down the menu, some ingredients are not single words:

```
...
Tonno: tomato sauce, tuna - 10€
Hawaii: tomato sauce, fresh pineapple, ham - 9€
...
```

Implement `oneIngredientParser` that will accept upper case and lower case ASCII characters or spaces `' '`.
Also take the chance to avoid a small flaw that `wordParser` had by making sure that empty strings are not recognized as valid ingredients and instead emit `Problem "empty string"`.
While you are at it, make sure to make the strings lowercase and also trim whitespace characters on both sides.

```elm
Parser.run oneIngredientParser "Tomato Sauce"
    --> Ok "tomato sauce"
Parser.run oneIngredientParser "   tomato sauce     "
    --> Ok "tomato sauce"
Parser.run oneIngredientParser ""
    --> Err [{ problem = Problem "empty string", col = 1, row = 1 }]
```

Once the parser is defined, use it in `ingredientsParser`.

## Source

### Created by

- @jiegillet

### Contributed to by

- @mpizenberg
- @ceddlyburge