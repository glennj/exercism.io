A caveat: I'm not a Lua programmer, though I did complete the track.

## iterating over a string

There is an alternate way to iterate over the characters of a string. The [`string.gmatch`](https://www.lua.org/manual/5.4/manual.html#pdf-string.gmatch) function returns an iterator so you can loop over all the matches. If your pattern is `"."` then each match is one character:

```lua
for char in input:gmatch(".") do
```

## exercise: leap

The key to this exercise is to combine the criteria into a single expression.
Use the [Logical Operators](https://www.lua.org/manual/5.4/manual.html#3.4.5).

I find that it helps to speak the criteria out loud using the words "and" "or" and "not": something like

> It is a leap year if X or Y and Z.

<details><summary>For some spoiler-y hints, click here...</summary>

---

One method to help you do this: if your cascading-if returns true for all the but last branch:
```lua
if first_thing then
    return true
elseif second_thing then
    return true
elseif third_thing then
    return true
else
    return false
end
```

Then you can join all the conditions with `or` into a single expression:
```lua
if first_thing or second_thing or third_thing then
    return true
else
    return false
end
```

And then once you have that, note that the single expression already evaluates to a boolean value. You can return it directly.
```lua
return first_thing or second_thing or third_thing 
```

</details>

---

This is a common anti-pattern:
```lua
if cond then
  return true
else
  return false
end
```
`cond` is already an expression that evaluates to a boolean value. You can return it directly.
```lua
return cond
```

<!-- -->

Can you combine the tests into one boolean expression (using [Logical operators](https://www.lua.org/manual/5.4/manual.html#3.4.5))?
It often helps to say the leap year rules out loud using the words "and" and "or" and "not".

# metatables and metamethods

https://exercism.org/mentoring/discussions/e43b7d8df6b348e48e39909bc5f2b815

I must admin I don't fully grok `__index`. Reading in the manual, it seems like it should be on each instance, not on the instance's metatable. I think it works like this
```lua
s = Set:new()
s:toString()
--[[
1. does table s have a toString key? no
2. does the metatable of s have a __index key? yes
3. does the metavalue of __index have a toString key? yes
4. ok, call that toString function.
--]]
```

# OO in Lua

OO in Lua was bolted on after the fact, and IMO it really shows.
I think the authors took some inspiration from Perl in how they did it.
* an object is initially just a simple datatype
* there's some magic syntax to link the datatype to the functions.

"Classes" and "objects" are Lua tables. 
Methods are implemented as functions stored in the class table.
Objects find their methods by setting their "metatable" to refer to the class.

[Simple Lua Classes][slc] in the lua-users wiki describes the technique.

## Two styles of OO

Lua has 2 common ways to implement OO: "closure-based" or "metatable-based".
See [the OO tutorial in the lua-users wiki](https://web.archive.org/web/20240104094707/http://lua-users.org/wiki/ObjectOrientationTutorial)

### Closure-based

Create a function that returns a table.
In this factory function, all the instance methods are added into the table.
Instance variables can be parameters to the factory function or local variables of it, hence the term "closure-based".

### Metatable-based

Create a table that acts as a "class".
This table holds all the methods.
One of the methods will create an instance (aka "new).
This creation method will create a table to act as the instance, 
and it will use the `setmetatable` function so the method lookup on the instance will be able to find methods defined on the class.
Instance variables are just entries in the instance table.

### Comparing

The closure-based approach has the advantage that instance variables are actually private.
The metatable-based approach relies on convention only that instance variable _should be_ private.

The metatable-based approach means that each object does not need to hold every method: 
they can be looked up in the class table.
The closure-based approach puts a copy of every method on every instance.

The closure-based approach _might be_ an easier concept to grasp:
there is no central "class", just a factory function that produces instances.
The metatable-based approach requires a bit of a deeper understanding of Lua [Metatables and Metamethods](https://www.lua.org/manual/5.4/manual.html#2.4)

Here's [a reddit thread](https://www.reddit.com/r/lua/comments/1al74ry/why_dont_more_people_suggest_closures_for_classes/) discussing this topic.

## Declaring classes

For declaring a class, use this convention:
```lua
local Robot = {}        -- 1. name the class
Robot.__index = Robot   -- 2. set the index

function Robot:new(attributes)
    local robot = {}    -- 3. name the new instance

    -- 4. set the metatable
    --    since `new` is called on the class, `self` is the class
    setmetatable(robot, self)   

    -- 5. do stuff with the attributes
    robot.name = attributes.name or "Anonymous"

    --    `robot` is a proper instance now, so it's OK to call methods.
    --    robot:location("assembly line")

    return robot        -- 6. return the new instance
end

local my_robot = Robot:new({name = "Robbie"})
```
And if you want to be able to create instances like
```lua
r = Robot(attributes)
```
you'd add
```lua
setmetatable(Robot, {
    __call = function(cls, ...) return cls:new(...) end,
})
```

Some more discussion about [Object Oriented Programming][oop] in Lua.

<!-- -->
Re: `__index` -- it works like this:

```lua
obj = SomeClass:new()
obj:someMethod()
--[[
1. does table obj have a someMethod key? no
2. does the metatable of obj have a __index key? yes
3. does the metavalue of __index have a someMethod key? yes
4. ok, call that someMethod function (with "self" set to obj).
--]]
```
<!-- -->

The colon notation is "syntactic sugar" that maps this call
```lua
someObj:someMethod(args)
```
to 
```lua
someMethod(someObj, args)
```

For defining a method, there are correspondingly 2 ways to do it.
```lua
function TheClass.aMethod(self, arg)
    self.someInstanceVar = arg
end
function TheClass:anotherMethod(arg)
    -- "self" is enforced as the instance
    self.someOtherInstanceVar = arg
end
```
Looking back at my solutions, I chose the latter style.

[Simple Lua Classes][slc] in the lua-users wiki favours the latter style.


[style]: https://web.archive.org/web/20240104094230/http://lua-users.org/wiki/LuaStyleGuide
[oop]: https://web.archive.org/web/20240104094707/http://lua-users.org/wiki/ObjectOrientedProgramming
[slc]: https://web.archive.org/web/20240104094707/http://lua-users.org/wiki/SimpleLuaClasses
