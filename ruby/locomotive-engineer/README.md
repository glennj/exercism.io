# Locomotive Engineer

Welcome to Locomotive Engineer on Exercism's Ruby Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

Decomposition refers to the act of extracting the elements of a collection, such as an `Array` or `Hash`.
Decomposed values can then be assigned to variables within the same statement.

[Multiple assignment][multiple assignment] is the ability to assign multiple variables to decompose values within one statement.
This allows for code to be more concise and readable, and is done by separating the variables to be assigned with a comma such as `first, second, third = [1, 2, 3]`.

The splat operator(`*`), and double splat operator, (`**`), are often used in decomposition contexts.

```exercism/caution
`*<variable_name>` and `**<variable_name>` should not be confused with `*` and `**`.
While `*` and `**` are used for multiplication and exponentiation, respectively, `*<variable_name>` and `**<variable_name>` are used as composition and decomposition operators.
```

## Multiple assignment

Multiple assignment allows you to assign multiple variables in one line.
To separate the values, use a comma `,`:

```irb
>> a, b = 1, 2
=> [1, 2]
>> a
=> 1
```

Multiple assignment is not limited to one data type:

```irb
>> x, y, z = 1, "Hello", true
=> [1, "Hello", true]
>> x
=> 1
>> y
=> 'Hello'
>> z
=> true
```

Multiple assignment can be used to swap elements in **arrays**.
This practice is pretty common in [sorting algorithms][sorting algorithms].
For example:

```irb
>> numbers = [1, 2]
=> [1, 2]
>> numbers[0], numbers[1] = numbers[1], numbers[0]
=> [2, 1]
>> numbers
=> [2, 1]
```

```exercism/note
This is also known as "Parallel Assignment", and can be used to avoid a temporary variable.
```

If there are more variables than values, the extra variables will be assigned `nil`:

```irb
>> a, b, c = 1, 2
=> [1, 2]
>> b
=> 2
>> c
=> nil
```

## Decomposition

In Ruby, it is possible to [decompose the elements of **arrays**/**hashes**][decompose] into distinct variables.
Since values appear within **arrays** in a index order, they are unpacked into variables in the same order:

```irb
>> fruits = ["apple", "banana", "cherry"]
>> x, y, z = fruits
>> x
=> "apple"
```

If there are values that are not needed then you can use `_` to indicate "collected but not used":

```irb
>> fruits = ["apple", "banana", "cherry"]
>> _, _, z = fruits
>> z
=> "cherry"
```

### Deep decomposing

Decomposing and assigning values from **arrays** inside of an **array** (_also known as a nested array_), works in the same way a shallow decomposing does, but needs [delimited decomposition expression (`()`)][delimited decomposition expression] to clarify the values context or position:

```irb
>> fruits_vegetables = [["apple", "banana"], ["carrot", "potato"]]
>> (a, b), (c, d) = fruits_vegetables
>> a
=> "apple"
>> d
=> "potato"
```

You can also deeply unpack just a portion of a nested **array**:

```irb
>> fruits_vegetables = [["apple", "banana"], ["carrot", "potato"]]
>> a, (c, d) = fruits_vegetables
>> a
=> ["apple", "banana"]
>> c
=> "carrot"
```

If the decomposition has variables with incorrect placement and/or an incorrect number of values, you will get a **syntax error**:

```ruby
fruits_vegetables = [["apple", "banana"], ["carrot", "potato"]]
(a, b), (d) = fruits_vegetables

syntax error, unexpected ')', expecting '.' or &. or :: or '['
((a, b), (d)) = fruits_vegetables
```

Experiment here, and you will notice that the first pattern dictates, not the available values on the right hand side.
The syntax error is not tied to the data structure.

### Decomposing an array with the single splat operator (`*`)

When [decomposing an **array**][decomposition] you can use the splat operator (`*`) to capture the "leftover" values.
This is clearer than slicing the **array** (_which in some situations is less readable_).
For example, we can extract the first element and then assign the remaining values into a new **array** without the first element:

```irb
>> fruits = ["apple", "banana", "cherry", "orange", "kiwi", "melon", "mango"]
>> x, *last = fruits
>> x
=> "apple"
>> last
=> ["banana", "cherry", "orange", "kiwi", "melon", "mango"]
```

We can also extract the values at the beginning and end of the **array** while grouping all the values in the middle:

```irb
>> fruits = ["apple", "banana", "cherry", "orange", "kiwi", "melon", "mango"]
>> x, *middle, y, z = fruits
>> y
=> "melon"
>> middle
=> ["banana", "cherry", "orange", "kiwi"]
```

We can also use `*` in deep decomposition:

```irb
>> fruits_vegetables = [["apple", "banana", "melon"], ["carrot", "potato", "tomato"]]
>> (a, *rest), b = fruits_vegetables
>> a
=> "apple"
>> rest
=> ["banana", "melon"]
```

### Decomposing a `Hash`

Decomposing a **hash** is a bit different than decomposing an **array**.
To be able to unpack a **hash** you need to convert it to an **array** first.
Otherwise there will be no decomposing:

```irb
>> fruits_inventory = {apple: 6, banana: 2, cherry: 3}
>> x, y, z = fruits_inventory
>> x
=> {:apple=>6, :banana=>2, :cherry=>3}
>> y
=> nil
```

To coerce a `Hash` to an **array** you can use the `to_a` method:

```irb
>> fruits_inventory = {apple: 6, banana: 2, cherry: 3}
>> fruits_inventory.to_a
=> [[:apple, 6], [:banana, 2], [:cherry, 3]]
>> x, y, z = fruits_inventory.to_a
>> x
=> [:apple, 6]
```

If you want to unpack the keys then you can use the `keys` method:

```irb
>> fruits_inventory = {apple: 6, banana: 2, cherry: 3}
>> x, y, z = fruits_inventory.keys
>> x
=> :apple
```

If you want to unpack the values then you can use the `values` method:

```irb
>> fruits_inventory = {apple: 6, banana: 2, cherry: 3}
>> x, y, z = fruits_inventory.values
>> x
=> 6
```

## Composition

Composing is the ability to group multiple values into one **array** that is assigned to a variable.
This is useful when you want to _decomposition_ values, make changes, and then _composition_ the results back into a variable.
It also makes it possible to perform merges on 2 or more **arrays**/**hashes**.

### Composition an array with splat operator(`*`)

Composing an **array** can be done using the splat operator, (`*`).
This will pack all the values into an **array**.

```irb
>> fruits = ["apple", "banana", "cherry"]
>> more_fruits = ["orange", "kiwi", "melon", "mango"]

# fruits and more_fruits are unpacked and then their elements are packed into combined_fruits
>> combined_fruits = *fruits, *more_fruits

>> combined_fruits
=> ["apple", "banana", "cherry", "orange", "kiwi", "melon", "mango"]
```

### Composition a hash with double splat operator(`**`)

Composing a hash is done by using the double splat operator(`**`).
This will pack all **key**/**value** pairs from one hash into another hash, or combine two hashes together.

```irb
>> fruits_inventory = {apple: 6, banana: 2, cherry: 3}
>> more_fruits_inventory = {orange: 4, kiwi: 1, melon: 2, mango: 3}

# fruits_inventory and more_fruits_inventory are unpacked into key-values pairs and combined.
>> combined_fruits_inventory = {**fruits_inventory, **more_fruits_inventory}

# then the pairs are packed into combined_fruits_inventory
>> combined_fruits_inventory
=> {:apple=>6, :banana=>2, :cherry=>3, :orange=>4, :kiwi=>1, :melon=>2, :mango=>3}
```

## Usage of splat operator(`*`) and double splat operator(`**`) with methods

### Composition with method parameters

When you create a method that accepts an arbitrary number of arguments, you can use [`*arguments`][arguments] or [`**keyword_arguments`][keyword arguments] in the method definition.
`*arguments` is used to pack an arbitrary number of positional (non-keyworded) arguments and
`**keyword_arguments` is used to pack an arbitrary number of keyword arguments.

Usage of `*arguments`:

```irb
# This method is defined to take any number of positional arguments
# (Using the single line form of the definition of a method.)

>> def my_method(*arguments)= arguments

# Arguments given to the method are packed into an array

>> my_method(1, 2, 3)
=> [1, 2, 3]

>> my_method("Hello")
=> ["Hello"]

>> my_method(1, 2, 3, "Hello", "Mars")
=> [1, 2, 3, "Hello", "Mars"]
```

Usage of `**keyword_arguments`:

```irb
# This method is defined to take any number of keyword arguments

>> def my_method(**keyword_arguments)= keyword_arguments

# Arguments given to the method are packed into a dictionary

>> my_method(a: 1, b: 2, c: 3)
=> {:a => 1, :b => 2, :c => 3}
```

If the method defined does not have any defined parameters for keyword arguments(`**keyword_arguments` or `<key_word>: <value>`) then the keyword arguments will be packed into a hash and assigned to the last parameter.

```irb
>> def my_method(a)= a

>> my_method(a: 1, b: 2, c: 3)
=> {:a => 1, :b => 2, :c => 3}
```

`*arguments` and `**keyword_arguments` can also be used in combination with one another:

```ruby
def my_method(*arguments, **keywword_arguments)
  p arguments.sum
  for (key, value) in keyword_arguments.to_a
    p key.to_s + " = " + value.to_s
  end
end


my_method(1, 2, 3, a: 1, b: 2, c: 3)
6
"a = 1"
"b = 2"
"c = 3"
```

You can also write arguments before and after `*arguments` to allow for specific positional arguments.
This works the same way as decomposing an array.

```exercism/caution
Arguments have to be structured in a specific order:

`def my_method(<positional_arguments>, *arguments, <positional_arguments>, <keyword_arguments>, **keyword_arguments)`

If you don't follow this order then you will get an error.
```

```ruby
def my_method(a, b, *arguments)
  p a
  p b
  p arguments
end

my_method(1, 2, 3, 4, 5)
1
2
[3, 4, 5]
```

You can write positional arguments before and after `*arguments`:

```irb
>> def my_method(a, *middle, b)= middle

>> my_method(1, 2, 3, 4, 5)
=> [2, 3, 4]
```

You can also combine positional arguments, \*arguments, key-word arguments and \*\*keyword_arguments:

```irb
>> def my_method(first, *many, last, a:, **keyword_arguments)
     p first
     p many
     p last
     p a
     p keyword_arguments
     end

>> my_method(1, 2, 3, 4, 5, a: 6, b: 7, c: 8)
1
[2, 3, 4]
5
6
{:b => 7, :c => 8}
```

Writing arguments in an incorrect order will result in an error:

```ruby
def my_method(a:, **keyword_arguments, first, *arguments, last)
  arguments
end

my_method(1, 2, 3, 4, a: 5)

syntax error, unexpected local variable or method, expecting & or '&'
... my_method(a:, **keyword_arguments, first, *arguments, last)
```

### Decomposing into method calls

You can use splat operator (`*`) to unpack an **array** of arguments into a method call:

```ruby
def my_method(a, b, c)
  p c
  p b
  p a
end

numbers = [1, 2, 3]
my_method(*numbers)
3
2
1
```

You can also use double splat operator(`**`) to unpack a **hash** of arguments into a method call:

```ruby
def my_method(a:, b:, c:)
  p c
  p b
  p a
end

numbers = {a: 1, b: 2, c: 3}
my_method(**numbers)
3
2
1
```

[arguments]: https://docs.ruby-lang.org/en/3.1/syntax/methods_rdoc.html#label-Array-2FHash+Argument
[keyword arguments]: https://docs.ruby-lang.org/en/3.1/syntax/methods_rdoc.html#label-Keyword+Arguments
[multiple assignment]: https://docs.ruby-lang.org/en/3.1/syntax/assignment_rdoc.html#label-Multiple+Assignment
[sorting algorithms]: https://en.wikipedia.org/wiki/Sorting_algorithm
[decompose]: https://docs.ruby-lang.org/en/3.1/syntax/assignment_rdoc.html#label-Array+Decomposition
[delimited decomposition expression]: https://riptutorial.com/ruby/example/8798/decomposition

## Instructions

Your friend Linus is a Locomotive Engineer who drives cargo trains between cities.
Although they are amazing at handling trains, they are not amazing at handling logistics or computers.
They would like to enlist your programming help organizing train details and correcting mistakes in route data.

```exercism/note
This exercise could easily be solved using slicing, indexing, and various `hash` methods.
However, we would like you to practice packing, unpacking, and multiple assignment in solving each of the tasks below.
```

## 1. Create an list of all wagons

Your friend has been keeping track of each wagon identifier (ID), but they are never sure how many wagons the system is going to have to process at any given time. It would be much easier for the rest of the logistics program to have this data packaged into a unified **array**.

Implement a method `generate_list_of_wagons()` that accepts an arbitrary number of wagon IDs.
Each ID will be a positive integer.
The method should then `return` the given IDs as a single **array**.

```ruby
LocomotiveEngineer.generate_list_of_wagons(1, 7, 12, 3, 14, 8, 5)
# => [1, 7, 12, 3, 14, 8, 5]
```

## 2. Fix the list of wagons

At this point, you are starting to get a feel for the data and how it's used in the logistics program.
The ID system always assigns the locomotive an ID of **1**, with the remainder of the wagons in the train assigned a randomly chosen ID greater than **1**.

Your friend had to connect two new wagons to the train and forgot to update the system!
Now, the first two wagons in the train **array** have to be moved to the end, or everything will be out of order.

To make matters more complicated, your friend just uncovered a second **array** that appears to contain missing wagon IDs.
All they can remember is that once the new wagons are moved, the IDs from this second **array** should be placed directly after the designated locomotive.

Linus would be really grateful to you for fixing their mistakes and consolidating the data.

Implement a method `fix_list_of_wagons()` that takes two **arrays** containing wagon IDs.
It should reposition the first two items of the first **array** to the end, and insert the values from the second **array** behind (_on the right hand side of_) the locomotive ID (**1**).
The method should then `return` a **array** with the modifications.

```ruby
LocomotiveEngineer.fix_list_of_wagons([2, 5, 1, 7, 4, 12, 6, 3, 13], [3, 17, 6, 15])
# => [1, 3, 17, 6, 15, 7, 4, 12, 6, 3, 13, 2, 5]
```

## 3. Add missing stops

Now that all the wagon data is correct, Linus would like you to update the system's routing information.
Along a transport route, a train might make stops at a few different stations to pick up and/or drop off cargo.
Each journey could have a different amount of these intermediary delivery points.
Your friend would like you to update the systems routing **hash** with any missing/additional delivery information.

Implement a method `add_missing_stops()` that accepts a routing **hash** followed by a variable number of keyword arguments.
These arguments could be in the form of a **hash** holding one or more stops, or any number of `stop_<number>: "city"` keyword pairs.
Your method should then return the routing **hash** updated with an additional **key** that holds a **array** of all the added stops in order.

```ruby
LocomotiveEngineer.add_missing_stops({from: "New York", to: "Miami"},
                    stop_1: "Washington, DC", stop_2: "Charlotte", stop_3: "Atlanta",
                    stop_4: "Jacksonville", stop_5: "Orlando")
# => {from: "New York", to: "Miami", stops: ["Washington, DC", "Charlotte", "Atlanta", "Jacksonville", "Orlando"]}
```

## 4. Extend routing information

Linus has been working on the routing program and has noticed that certain routes are missing some important details.
Initial route information has been constructed as a **hash** and your friend would like you to update that **hash** with whatever might be missing.
Every route in the system requires slightly different details, so Linus would really prefer a generic solution.

Implement a method called `extend_route_information()` that accepts two **hashes**.
The first **hash** contains the origin and destination cities the train route runs between.

The second **hash** contains other routing details such as train speed, length, or temperature.
The method should return a consolidated **hash** with all routing information.

```exercism/note
The second **hash** can contain different/more properties than the ones shown in the example.
```

```ruby
LocomotiveEngineer.extend_route_information({"from": "Berlin", "to": "Hamburg"}, {"length": "100", "speed": "50"})
# => {"from": "Berlin", "to": "Hamburg", "length": "100", "speed": "50"}
```

## Source

### Created by

- @meatball133
- @kotp