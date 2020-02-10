
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

<!-- ===================================================== -->
## Exercise: two-fer

Instead of hardcoding the output strings in two places, have the `who`
method call the `who: aString` method, passing it the default value.
