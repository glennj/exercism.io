# Leslie's Lengthy Lists

Welcome to Leslie's Lengthy Lists on Exercism's Common Lisp Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Lists

Given that the name of the language is Lisp which stands of _LISt Processing_ one might assume that the language has facilities for handling lists of items, and you'd be correct!

While Common Lisp has other data structures as well as lists, lists are still heavily used.

A list in Common Lisp is a sequence of items.
The items themselves do not have to be the same type.
For example you can have a list of `1`, `two`, `"III"`.

### Creating Lists

One can simply type in a quoted list like this: `'(1 two "III")` and that will cause a list to be created and evaluated (it evaluates to: `(1 two "III")`.

There are also two main functions used to create lists: `list` and `cons`.

`list` takes zero or more arguments and evaluates to a list created with those values:

```lisp
(list 1 'two "III") ; => (1 two "III")
```

`cons` takes two items and creates a list which has as its `car` the first item and as its `cdr` the second item:

```lisp
(cons 1 2)            ; => (1 . 2) ;; (a list without `nil` as its `cdr` is printed in this way.)
(cons 1 nil)          ; => (1)
(cons 1 (cons 2 nil)) ; => (1 2)
```

`car` and `cdr` can be used to access the `car` and `cdr` respectively.

(`first` and `rest` are synonyms of `car` and `cdr` and work exactly the same.)

### Length & Random Access

The length of a list can be determined by the use of `length`.
An empty list has length zero.

An arbitrary item can be accessed with `nth` (note that lists are zero-indexed).

It is _not_ an error to request an index that is more than the length.
Instead it evaluates to `nil`:

```lisp
(nth 23 '(short list))` ; => nil
```

There are also 10 helper methods for accessing the first 10 items of a list, they are named: `first`, `second`, `third`, `fourth`, `fifth`, `sixth`, `seventh`, `eighth`, `ninth`, and `tenth`.

### Combining lists

Two, or more, lists can be combined with `append`: 

```lisp
(append '(a b c) '(1 2 3)) ; => (A B C 1 2 3)
(append '(a b c) '()))     ; => (A B C)
```

Each argument given to `append` needs to be a list.

## Instructions

Leslie the Lisp Alien needs to do some shopping. It is very important to have a shopping list. One needs to add things to it, and remove things from it.

Of course simple pen and paper will not do for a Lisp Alien. "List" is most of the word "Lisp" even!. There must be some functions written to help keep track of the shopping.

Can you help Leslie keep track of the shopping list?

## 1. Making a new list

First thing is that Leslie needs to create a empty list. A function called `new-list` would be perfect for that.

```lisp
(new-list) ; => ()
```

Oh no... Leslie has a few things in mind already, so they need a function that takes three items (luckily Leslie only creates a list of three items. Nothing more, nothing less!) and creates a new shopping list with those things. Write a function, `list-of-things` which takes three items and returns a list of them.

```lisp
(list-of-things 'bread 'milk 'butter) ; => '(bread milk butter)
```

## 2. Add things to the list.

Before going to the store Leslie looks in the pantry to see what they need. Help them by writing the function `add-to-list` which adds an item to the beginning of a list.

```lisp
(add-to-list 'butter '(bread)) ; => '(butter bread)
```

## 3. What's next thing(s) on the list?

When shopping, Leslie wants to know what to look for next. They also like to peek ahead at the list to see the second, third, or even the 23rd item (their lucky number).

- `first-thing` will evaluate to the first thing on the list
- `second-thing` will evaluate to the second thing
- `third-thing` will evaluate to the third thing
- `twenty-third-thing` will evaluate to the twenty-third thing

```lisp
(first-thing '(bread butter milk)) ; => 'bread
(second-thing '(bread butter milk)) ; => 'butter
(third-thing '(bread butter milk)) ; => 'milk
```

## 4. Removing a thing from the list

Leslie wants to find the first item of the list and remove the it from the shelf.
Help them out by writing a function `remove-first-item` which evaluates to a list with everything but the first item on the input list.

```lisp
(remove-first-item '(bread butter milk)) ; => '(butter milk)
```

## 5. Bigger lists out of smaller lists

Leslie realized they accidentally made two shopping lists, not one! Write a function, `list-append` which returns a list that with the elements from the first list followed by the ones from the second list.

```lisp
(list-append '(bread salt) '(butter milk)) ; => '(bread salt butter milk)
```

## 6. How much longer?

Leslie is getting worried that this shopping trip will take a while. Just how many things are on this list? Write a function `just-how-long` to tell them how long their list is.

```lisp
(just-how-long '(bread milk butter salt)) ; => 4
```

## Source

### Created by

- @verdammelt