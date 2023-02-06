A caveat: I'm not a Lua programmer, though I did complete the track.

# OO in Lua

OO in Lua was bolted on after the fact, and IMO it really shows.
I think the authors took some inspiration from Perl in how they did it.
* an object is initially just a simple datatype
* there's some magic syntax to link the datatype to the functions.

The colon notation is "syntactic sugar" that maps this call
```lua
someObj:someMethod(args)
```
to 
```lua
someMethod(someObj, args)
```
We can see why `self` needs to be the first argument.

Since there are 2 ways to do it, just pick a style and be consistent with it.
I don't know what the Lua community recommends: the [Lua Style Guide][style] doesn't have much to say about it.
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

## Declaring classes

For declaring a class, I tried to follow this convention:
```lua
local Robot = {}        -- 1. name the class
Robot.__index = Robot   -- 2. set the index

function Robot:new(attributes)
    local robot = {}    -- 3. name the new instance

    -- 4. set the metatable
    --    since `new` is called on the class, `self` is the class
    setmetatable(robot, self)   

    -- 5. do stuff with the attributes
    --    `robot` is a proper instance now, so it's OK to call methods.
    --    +> robot:location("assembly line")

    return robot        -- 6. return the new instance
end

local my_robot = Robot:new({name: "Robbie"})
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


[style]: http://lua-users.org/wiki/LuaStyleGuide
[oop]: http://lua-users.org/wiki/ObjectOrientedProgramming
