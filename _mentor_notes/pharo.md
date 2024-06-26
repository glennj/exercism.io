Be sure to check out [the community solutions](https://exercism.io/tracks/pharo-smalltalk/exercises/__SLUG__/solutions) to see other approaches.

## Submitting in Pharo IDE

A student was able to submit, but [only the test results were sent to
exercism, not the actual code](https://exercism.io/mentor/solutions/8e1fe71d8b7a402bb011a08335a6ab0e?iteration_idx=1#discussion-post-975220:):

> reason why my code was not being submitted was that i had subclassed from #ExercismTest
> assuming that was required. Subclassed from #Object and code shows up in exercism

## super vs self

Super allows you to access methods from ancestor classes. For example you might have this:
```smalltalk
Class {
    #name : #Vehicle,
    #superclass : #Object,
    #instVars : [ 'motor' ]
}

Vehicle >> initialize [
    motor := true
]
```
Then a subclass
```smalltalk
Class {
    #name : #Car,
    #superclass : #Vehicle,
    #instVars : [ 'numWheels' ]
}

Car >> initialize [
    numWheels := 4
]
```
When you do `Car new`, the new instance will have `motor` value `nil` -- unitialized. We haven't called the parent's initialize method.
```smalltalk
Car >> initialize [
    super initialize.
    numWheels := 4
]
```

I think it boils down to this:
super **is** self, except the method lookup _starts in the superclass_.

## block shorthand

A reminder that when you have a block like `[ :x | x method ]` you can provide just the method name as a symbol `#method`:
```smalltalk
aString select: [ :c | c isLetter ] thenCollect: [ :c | c lowercase].
" or "
aString select: #isLetter thenCollect: #lowercase.
```

## implementing `=`

My understanding that classes that implement `=` should also implement hash.
* Two objects that are equal must have the same hash.
* Two objects that are unequal may or may not have different hash values.

I believe the best practice for implementing = is:
* test if the objects are the same object (using the ProtoObject == method)
* test if they are the same `species`
* then do equality test based on instance vars.

## and: and or: take blocks

I frequently see this:
```smalltalk
(something and: (something else)) ifTrue: [ do stuff ].
```
Be aware that the argument to `and:` and `or:` is a **block** not an
expression. You must supply a block to achieve the short-circuiting effect
you're after. If you supply an expression instead, Smalltalk will *evaluate
the expression first*, and use that value as the argument to `and:`

Consider this contrived example where the `and:` argument actually mutates
an object:
```smalltalk
| myList |
myList := OrderedCollection new.

(false and: ( (myList add: 42; yourself) size = 0 )) 
	ifTrue: [Transcript show: 'this is true'; cr]
	ifFalse: [Transcript show: 'this is false'; cr].

myList inspect.
```
We see the "this is false" message on the transcript. However,
since the receiver of `and:` is `false`, we would expect myList to empty
afterwards: it is not.

We must use proper block syntax:
```smalltalk
" ..........v.....................................v "
(false and: [ (myList add: 42; yourself) size = 0 ]) 
```

## Instance vars verses Class var/method

<!-- ref https://exercism.io/mentor/solutions/571b9fbdee9f44688173c39aa02c4a6d -->
Since it's a constant, the timeDict instance variable could also be written as a class variable or class method.

<!-- ===================================================== -->
## Exercise: two-fer

Smalltalk values code reuse as much as possible. In this exercise, the `who`
function can invoke the `who: aName` function, passing the default value.

<!-- ===================================================== -->
## Exercise: leap

Be aware that the argument to `and:` and `or:` is a **block** not an
expression. If you supply an expression, Smalltalk will *evaluate
the expression first*, and use that value as the argument to `and:`.
Here, if the year is not divisible by 4, there's no need to divide it by 100
and then 400.

<!-- ===================================================== -->
## Exercise: space-age

Instantiate an object given the **name** of a class:
```smalltalk
SpaceAge >> ageOnPlanet: planet at: seconds [
	^ (self class environment at: planet asSymbol) new
		seconds: seconds;
		age
]
```
could be:
```smalltalk
SpaceAge >> ageOnPlanet: planet at: seconds [
	^ planet asClass new
		seconds: seconds;
		age
]
```
But as [@moniquelive mentioned](https://exercism.org/mentoring/discussions/854b5ce624064dd5bc30fc9d0ba5af94):

> I used `planetStr asClass new` first, but then Pharo warned me to use this call instead :)))

<!-- ===================================================== -->
## Exercise: reverse-string

There is a convenience method for
```smalltalk
    stream := WriteStream on: (String new: aSize). 
    "add stuff to stream"
    ^ stream contents.
```
It's the `streamContents:` method
```smalltalk
    ^ String new: aSize
        streamContents: [:stream |
            "add stuff to stream"
        ].
```
The block implicitly returns the stream contents when complete.
The underlying implementation is pretty much exactly the same as the first code snippet.

<!-- ===================================================== -->
## Exercise: roman-numerals

Strings are immutable.
Appending to a string will build a new string.
This can affect memory and performance.
In the community solutions, you'll see the use of `String streamContents: [:aStream | ...]` -- this is like a StringBuilder you'll see in other languages.
In the block, you'll typically use `aStream nextPut: something` or `aStream nextPutAll: someCollection` to add to the stream.
