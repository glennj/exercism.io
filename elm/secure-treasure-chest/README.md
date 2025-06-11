# Secure Treasure Chest

Welcome to Secure Treasure Chest on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Opaque Types

### Introduction

Opaque Types are an encapsulation technique in Elm.
Some people also call this information hiding, or hiding implementation details.
They are also commonly used to enforce validation, using the [Parse, dont validate][parse-dont-validate] pattern.
For example, you could have a `EmailAddress` opaque type, that can only contain valid email addresses (as opposed to a `String`, which can contain anything).

### Creating an Opaque Type

[Custom types][custom-types] have one or more *variants*, and the compiler automatically generates a function to create each *variant*, using the *variant* name.
These are the only way in which custom types can be created.
The compiler also allows destructuring for these variants.

```elm
type Maybe a
    = Nothing --> Variant implies a 'Nothing' function, with type 'Maybe a', and destructuring of 'Nothing'
    | Just a  --> Variant implies a 'Just' function, with type 'a -> Maybe a', and destructuring of 'Just _'
```

By default, all of this is internal to the file / module, and we use the `exposing` keyword to choose what to export.
To create an Opaque Type, you simply choose not to export the *variants*.

When you do this, you almost always need to supply another way to create and get the *variants*, by exposing custom functions.
These custom functions allow you to apply some logic when creating and getting the *variants*.
This hides the internal implementation details of the module, and means that you can change the implementation details as much as you want, as long as the exposed functions remain the same.

### Parse, don't validate

Probably the most common use of Opaque Types is to enforce validation, using the [Parse, dont validate][parse-dont-validate] pattern.
In this pattern, you expose a function that takes an input with less structure / type safety than you want, such as a `String`, and then parse this in to a more structured type, such as `EmailAddress`, either returning the type, or returning an error (usually `Maybe` or `Result`).

For example, in the code below, it is only possible for external modules to create a `EmailAddress` type using the `parseEmailAddress` function, so when you are working with the `EmailAddress` type, you are certain that it is valid.

```elm
module EmailAddress exposing ( EmailAddress, parseEmailAddress, getEmailAddress )

-- The `EmailAddress` custom type has one variant, also called `EmailAddress`
-- When a custom type has one variant, it is idiomatic to use the same name for both.
type EmailAddress = 
  EmailAddress String

-- create an EmailAddress variant
parseEmailAddress: String -> Maybe EmailAddress
parseEmailAddress candidateEmailAddress =   
  if isValidEmailAddress candidateEmailAddress then
    Just (EmailAddress candidateEmailAddress)
  else
    Nothing

-- get an EmailAddress variant
getEmailAddress: EmailAddress -> String
getEmailAddress ( EmailAddress  emailAdress ) =
  emailAdress

isValidEmailAddress: String -> bool
-- ... 
```

[custom-types]: https://guide.elm-lang.org/types/custom_types.html
[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
[uphold-invariants]: https://ckoster22.medium.com/advanced-types-in-elm-opaque-types-ec5ec3b84ed2

## Instructions

In this exercise you're going to write a generic (/ magical!) `SecureTreasureChest`, to store some treasure.
Sharp eyed students will have noticed that the previous `TreasureChest` wasn't that secure!
If you used the `getTreasure` function, then you had to supply the password, but you could still simply destructure the `TreasureChest` type to get the treasure, without having to know the password.
Even worse, you could even retrieve the password in the same way!
Let's fix that by using an Opaque Type.

## 1. Define the Password opaque type, using the "Parse, don't validate" pattern

Define a `Password` custom type with a single variant.

The variant should have an associated `String` value, for the password.

You should expose the `Password` type, but not the variant.
This makes `Password` an Opaque type, and means that you can no longer use destructing to get the password.
It is not possible to test whether a type is Opaque or not, in production code, you would use [elm-review][elm-review] (a static analysis tool) to enforce it.

Define and expose a `createPassword` function, with a type definition, that takes a `String` and returns a `Maybe Password`.

Owasp recommends that [passwords should be at least 8 characters long][owasp-password-length], so lets use the [Parse, don't validate][parse-dont-validate] pattern to enforce this invariant.

If the password is less than 8 characters long, `createPassword` should return `Nothing`, otherwise it should return `Just Password`.

## 2. Expose SecureTreasureChest, but not it's variant

You should expose the `SecureTreasureChest` type, but not the variant.
This makes `SecureTreasureChest` an Opaque type, and means that you can no longer use destructuring to get the password or the treasure.

## 3. Define the createTreasure function

This function should take two parameters, and have a type definition.

- a generic type (for the treasure)
- a `Password` (for the password)

The function should return a `SecureTreasureChest`.

Expose this function, so that other modules are able to create instances of `SecureTreasureChest`.

## 4. Define the getTreasure function

This function should take two parameters, and have a type definition.

- a `String` (for the password)
- a `SecureTreasureChest` generic custom type

This function should check the provided password attempt against the `Password` in the `SecureTreasureChest`.

The function should return a `Maybe`.

If the passwords match then return `Just` with the generic value from the `SecureTreasureChest` (the treasure!)

If the passwords do not match then return `Nothing`.

Expose this function, so that other modules are able to get the treasure from a `SecureTreasureChest`, as long as they supply the correct password.


[owasp-password-length]: https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html#implement-proper-password-strength-controls
[elm-review]: https://package.elm-lang.org/packages/jfmengels/elm-review/latest/
[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

## Source

### Created by

- @ceddlyburge
- @jiegillet