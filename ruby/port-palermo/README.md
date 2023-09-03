# Port Of Palermo

Welcome to Port Of Palermo on Exercism's Ruby Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

[Symbols][symbols] are named identifiers that can be used to refer to a value.
Symbols are created through a symbol literal, which is by prefixing a name with a `:` character, e.g. `:foo`.
They also allow for being written with quotes, e.g. `:"foo"`, which allows, for example, spaces in the name.

```ruby
:foo # => :foo
:"foo boo" # => :"foo boo"
```

Symbols are used in many places in the language, including as keys in hashes, to represent method names and variable names.

## Identifier

What makes symbols different from strings is that they are identifiers, and do not represent data or text.
This means that two symbols with the same name are always the same object.

```ruby
"foo".object_id # => 60
"foo".object_id # => 80
:foo.object_id # => 1086748
:foo.object_id # => 1086748
```

## Modifying Symbols

Symbols are immutable, which means that they cannot be modified.
This means that when you "modify" a symbol, you are actually creating a new symbol.
There are a few methods that can be used to manipulate symbols, they all return new symbols.
All methods can be found in the [Symbol API][symbols-api].

```ruby
:foo.upcase # => :FOO

:foo.object_id # => 1086748
:foo.upcase.object_id # => 60
```

The benefit of symbols being immutable is that they are more memory efficient than strings, but also safer to use as identifiers.

## Conversion

Symbols can be converted to strings and vice versa.
This can be useful when you want to modify a symbol, or when you want to use a symbol as a string.
To present a symbol as a string, you can use the `String#to_sym` method, and to do the opposite, you can use the `Symbol#to_s` method.
Due to symbols having a limited set of methods, it can be useful to convert a symbol to a string to use string methods on it, if a new symbol is needed.

```ruby
:foo.to_s # => "foo"
"foo".to_sym # => :foo
```

[symbols]: https://www.rubyguides.com/2018/02/ruby-symbols/
[symbols-api]: https://rubyapi.org/o/symbol

## Instructions

The port of Palermo is a busy harbor, with a lot of ships coming and going.
It has for a long time had a manual system for keeping track of the ships that are currently in the harbor.
This system is very error prone, and the harbor chief has decided to replace it with a computerized system.

The harbor chief has hired you to implement the new system.
The system has to handle identifiers for ships, but also for destinations.

## 1. Create the ports identifier

The first thing you need to do is to create the identifier for the port of Palermo.
The identifier are the first four letters of the name of the port, in uppercase.

Define the `Port::IDENTIFIER` constant to be a symbol with the value `:PALE`.
    
```ruby
Port::IDENTIFIER 
# => :PALE
```

## 2. Get identifiers for other ports

The program will also have to handle other ports, so you need to create identifiers for them as well.
The port would like the system to be automated and since ports uses different naming conventions, the identifiers should be generated from the name of the port.
The identifier are the first four letters of the name of the port, in uppercase.

Define the `Port.get_identifier` method to take a string as an argument, and returns the identifier as a symbol for that port.

```ruby
Port.get_identifier("Hamburg") 
# => :HAMB
```

## 3. Determine which terminal a ship should dock at

The port has two terminals, and the system needs to determine which terminal a ship should dock at.
The terminal is determined by the identifier of the ship.

The identifier is built of 2 parts, the first part is made of 3 uppercased letters which says which cargo the ship is carrying, and then 3 numbers which is the unique identifier of the ship.

If the identifier of the ship is carrying: **OIL** or **GAS** the ship should dock at terminal **A**.
Else the ship should dock at terminal **B**. 

Define the `Port.get_terminal` method to take a symbol as an argument which is the identifier of the ship.
The method should return the terminal as a symbol.

```ruby
Port.get_terminal(:OIL123)
# => :A
```

## Source

### Created by

- @meatball133

### Contributed to by

- @egemen-dev