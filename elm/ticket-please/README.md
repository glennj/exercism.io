# Ticket, Please!

Welcome to Ticket, Please! on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Pattern Matching

[Pattern matching][pattern-matching] enables expressive branching code and [destructuring][destructuring] brings elegant binding of values to variables.

### Simple pattern matching

Pattern matching can bind values to variables, or discard them with the wildcard `_`, with the use of the `case` statement.

```elm
type Entity = Alien | Stranger (Maybe String) | Friend { name: String }

hello : Entity -> String
hello entity =
    case entity of
        -- Custom type variant
        Alien -> "Hello, you are not from around here are you?"
        -- Binding to a litteral value nested in Stranger
        Stranger (Just nametag) -> "Hello, erm... " ++ nametag ++ "."
        -- Discarding Friend's data
        Friend _ -> "Hi!"
        -- All other cases
        _ -> "Hello stranger!"
```

### Destructuring

Destructuring can binds values in `let` bindings, function arguments, and of course in case expressions, whenever there is only one shape possible for the data.

```elm
pairSum : ( Int, Int ) -> Int
pairSum pair =
    -- Destructuring of a pair in a 'let' binding
    let ( x, y ) = pair
    in x + y

-- Destructuring in the function argument
pairSum : ( Int, Int ) -> Int
pairSum ( x, y ) = x + y

-- Custom type containing a single variant
type Container = Box String

-- Destructuring in the function argument
unbox : Container -> String
unbox (Box str) = str

-- Destructuring combined with pattern matching
unboxMaybe : Maybe Container -> Maybe String
unboxMaybe maybeContainer =
    case maybeContainer of
        Nothing -> Nothing
        Just (Box "42") -> Just "The answer to the universe!"
        Just (Box str) -> Just str
```

Destructuring can also be used for [records][records-pattern-matching], or with the [`as` keyword][as-keyword].

```elm
type alias Circle =
    { radius : Float
    , center : ( Float, Float )
    }

perimeter : Circle -> Float
perimeter { radius } =
    2 * pi * radius

left : Circle -> Float
left { radius, center } =
    -- the pair center cannot be pattern matched directly in the function argument
    -- so we do it in a 'let' statement
    let ( x, _ ) = center
    in x - radius

-- using the 'as' keyword to bind both the fields and the whole record
smaller : Float -> Circle -> Circle
smaller reduction ({ radius } as circle) =
    if reduction < radius then
        { circle | radius = radius - reduction }
    else
        circle
```

[pattern-matching]: https://guide.elm-lang.org/types/pattern_matching.html
[destructuring]: https://gist.github.com/yang-wei/4f563fbf81ff843e8b1e
[records-pattern-matching]: https://elm-lang.org/docs/records#pattern-matching
[as-keyword]: https://github.com/izdi/elm-cheat-sheet#operators

## Instructions

You are part of the IT helpdesk, and you want to improve the ticketing system to be more efficient and provide statistics for your manager.

A ticket has a status (new, in progress...), a creator (identified by a username and an employee ID), can be assigned to an IT employee and contains a history of comments.

Here are four tasks that wil improve your current ticketing system.

## 1. Detect empty comments

Empty comments clutter the interface and bring no information, you want to filter them out.

Define a helper function `emptyComment` that take a user/comment pair and return `True` for empty comments.

Use destructuring to extract the user and the comment directly in the function argument. Ignore the user since you don't need that information.

```elm
emptyComment ( User "Alice", "" )
-- => True
```

## 2. Count the number of comments from the creator

Your manager wants to have statistics about user involvement in the ticket process. 
She asks you to check how many comments are added by the ticket creator.

Define `numberOfCreatorComments` that takes a ticket and return the number of comments added by the ticket creator.

Use destructuring in the function argument to extract the record from the `Ticket` type, at the same time as the fields you need from the record. 
Then, destructure the `createdBy` in a `let` binding so you can count the relevant comments.
Use `List` functions to do the count and use destructuring in the function argument of an anonymous function to seperate the user/comment pair.

```elm
numberOfCreatorComments
numberOfCreatorComments
    (Ticket
        { status = Closed
        , createdBy = ( User "Chiaki", 1 )
        , assignedTo = Nothing
        , comments = [ ( User "Chiaki", "Never mind, fixed it." ) ]
        }
    )
-- => 1
```

## 3. Detect tickets assigned to the Dev Team

Some tickets have to be escalated to Alice, Bob, or Charlie from the Dev Team.
It's important to keep an eye on those to make sure they are making progress.

Define a helper function `assignedToDevTeam` that checks if a ticket is assigned to one of the Dev Team members.

Use destructuring in the function argument to extract the record from the `Ticket` type as well as the field you need.
Then, use a single `case` statement to check if the ticket is assigned to Alice, Bob, or Charlie using recursive pattern matching to extract their usernames and litteral pattern matching to match them. 

```elm
assignedToDevTeam
    (Ticket
        { status = InProgress
        , createdBy = ( User "Bill", 2 )
        , assignedTo = Just (User "Alice")
        , comments = [ ( User "Bill", "What's an 'undefined'?" ) ]
        }
    )
-- => True
```

## 4. Assign tickets to IT employees

Sometimes a bunch tickets need to be reassigned to other IT employees.

Define a function `assignTicketTo` to help you do that. The function receives two arguments: a user to assign the ticket to and a ticket. 
If the status of the ticket is `New`, assign it to the new user and change the status to `InProgress`. 
For other statuses, simply reassign the ticket to the new users, except for tickets with the status `Archived` that should not be modified at all.

Use destructuring in the function argument to extract the record from the `Ticket` type as well as the field you need, and also use the `as` keyword to capture the whole record as it is. 
Use a single `case` statement to pattern match the status and return the appropriate ticket, created from modifying the original record you captured with the `as` keyword.

```elm
assignTicketTo (User "Danny")
    (Ticket
        { status = New
        , createdBy = ( User "Jesse", 3 )
        , assignedTo = Just (User "Alice")
        , comments = [ ( User "Jesse", "I've been waiting for 6 months!!" ) ]
        }
    )
-- => Ticket
--        { status = InProgress
--        , createdBy = ( User "Jesse", 3 )
--        , assignedTo = Just (User "Danny")
--        , comments = [ ( User "Jesse", "I've been waiting for 6 months!!" ) ]
--        }
```

## Source

### Created by

- @mpizenberg
- @jiegillet

### Contributed to by

- @ceddlyburge