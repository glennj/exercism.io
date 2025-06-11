# Paula's Palindromes

Welcome to Paula's Palindromes on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Web Applications

Elm is a delightful language for building reliable web applications.
It has friendly error messages, great performance, small assets, and no runtime exceptions.

Probably the most famous and widely copied part of Elm is The Elm Architecture, which is a simple pattern for structuring web applications, and is how all Elm web applications are written.

The core idea is that your code is built around a `Model` of your application state, a way to `update` your model, and a way to `view` your model.

The `Model` contains the application’s state - all the data that the application needs.
If you imagine a simple application with a text box, and text showing the reverse of that text, the model would look like below.
You can also see this example on the [Elm Guide][elm-guide-text-fields].

```elm
type alias Model =
  { text : String
  }
```

`update` is a function that gets called with a `Msg` when there's a change (like the user clicking a button).
It takes the `Msg` and the current `Model`, and returns a new `Model`.

```elm
type Msg
  = TextChanged String

update : Msg -> Model -> Model
update msg model =
  case msg of
    TextChanged newText ->
      { model | text = newText }
```

`view` is a function that returns HTML to show to the user in the browser.
It takes the current `Model` and returns an `Html Msg` (the type that Elm uses to represent HTML).

```elm
view : Model -> Html Msg
view model =
  Html.div []
    [ Html.input
      [ Html.Attributes.placeholder "Text to reverse"
      , Html.Attributes.value model.text
      , Html.Events.onInput TextChanged
      ]
      []
    , Html.div [] [ Html.text (String.reverse model.text) ]
    ]
```

[elm-guide-text-fields]: https://guide.elm-lang.org/architecture/text_fields

## Instructions

Your friend Paula is trying to get better at solving crossword clues. She has read that [some clues show that the answer is a palindrome][palindrome-crossword-clues].
A palindrome is a word or sentence that reads the same backards as forwards.

She asks you write her a very simple website that will check whether a string is a palindrome (or not) so that she can check her guesses.

You decide to do this using an Elm [sandbox][browser-sandox] application.

The application will have a text box, that Paula can type her guess into, and text stating whether or not the text in the box is a palindrome.
The text will update whenever the text in the text box changes.

## 1. Define the `Model` and `Msg` types for the application, and write the `init` function

The application `Model` needs the string to check, this field should be called `content`.

The application needs a `Msg` to indicate that the text in the Text Box has changed.
The variant for this should be called `Change`

The `init` function should return a initial `Model` value with an empty `content` field.

## 2. Write the update function

The `update` function should take a `Msg` and a `Model` parameter, and return a new `Model`.
The `Msg` will carry some text typed by the user, and the new `Model` should include that text.

## 3. Write the view function

Elm requires that the view function has to return an `Html msg` type, which means that you need to return a single element from the view function (and not an  array of elements).

Inside this root element, there should be an `input` element (the Text Box), and a `div` with `text` content to state whether the text in the `input` is a palindrome or not.

The text should be "This is a palindrome" or "Not a palindrome".

## 4. Write the main function

The `main` function should call [`Browser.sandbox`][browser-sandbox], passing a record parameter with the `init`, `update` and `view` functions.
To make the exercise work on the Exercism online editor, we use a fake `Browser.sandbox` with the code.
This makes no difference to the code that you write to solve this exercise.

[palindrome-crossword-clues]: https://www.theguardian.com/crosswords/crossword-blog/2012/nov/01/cryptic-crosswords-beginners-palindromes
[browser-sandbox]: https://package.elm-lang.org/packages/elm/browser/latest/Browser#sandbox

## Source

### Created by

- @ceddlyburge