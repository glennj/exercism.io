A caveat: I'm not a Lua programmer, though I did complete the track.

# OO in Lua

OO in Lua was bolted on after the fact, and IMO it really shows.
I think the authors took some inspiration from Perl in how they did it.
* an object is initially just a simple datatype
* there's some magic syntax to link the datatype to the functions.

"Classes" and "objects" are Lua tables. 
Methods are implemented as functions stored in the class table.
Objects find their methods by setting their "metatable" to refer to the class.

[Simple Lua Classes][slc] in the lua-users wiki describes the technique.


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


[style]: http://lua-users.org/wiki/LuaStyleGuide
[oop]: http://lua-users.org/wiki/ObjectOrientedProgramming
[slc]: http://lua-users.org/wiki/SimpleLuaClasses
