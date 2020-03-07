# Rail Fence Cipher

Implement encoding and decoding for the rail fence cipher.

The Rail Fence cipher is a form of transposition cipher that gets its name from
the way in which it's encoded. It was already used by the ancient Greeks.

In the Rail Fence cipher, the message is written downwards on successive "rails"
of an imaginary fence, then moving up when we get to the bottom (like a zig-zag).
Finally the message is then read off in rows.

For example, using three "rails" and the message "WE ARE DISCOVERED FLEE AT ONCE",
the cipherer writes out:

```text
W . . . E . . . C . . . R . . . L . . . T . . . E
. E . R . D . S . O . E . E . F . E . A . O . C .
. . A . . . I . . . V . . . D . . . E . . . N . .
```

Then reads off:

```text
WECRLTEERDSOEEFEAOCAIVDEN
```

To decrypt a message you take the zig-zag shape and fill the ciphertext along the rows.

```text
? . . . ? . . . ? . . . ? . . . ? . . . ? . . . ?
. ? . ? . ? . ? . ? . ? . ? . ? . ? . ? . ? . ? .
. . ? . . . ? . . . ? . . . ? . . . ? . . . ? . .
```

The first row has seven spots that can be filled with "WECRLTE".

```text
W . . . E . . . C . . . R . . . L . . . T . . . E
. ? . ? . ? . ? . ? . ? . ? . ? . ? . ? . ? . ? .
. . ? . . . ? . . . ? . . . ? . . . ? . . . ? . .
```

Now the 2nd row takes "ERDSOEEFEAOC".

```text
W . . . E . . . C . . . R . . . L . . . T . . . E
. E . R . D . S . O . E . E . F . E . A . O . C .
. . ? . . . ? . . . ? . . . ? . . . ? . . . ? . .
```

Leaving "AIVDEN" for the last row.

```text
W . . . E . . . C . . . R . . . L . . . T . . . E
. E . R . D . S . O . E . E . F . E . A . O . C .
. . A . . . I . . . V . . . D . . . E . . . N . .
```

If you now read along the zig-zag shape you can read the original message.


## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.

## Running the tests
To run the test suite, execute one of the following commands:

```bash
tclsh rail-fence-cipher.test            # Will stop testing after the first failure.
RUN_ALL=1 tclsh rail-fence-cipher.test  # Will run all tests and report all failures.
```

## Feedback, Issues, Pull Requests
The [exercism/tcl](https://github.com/exercism/tcl) repository on GitHub is
the home for all of the Tcl exercises on Exercism.

If you have feedback about an exercise, or want to help implementing a new
one, head over there and create an issue.  We'll do our best to help you!

## Source

Wikipedia [https://en.wikipedia.org/wiki/Transposition_cipher#Rail_Fence_cipher](https://en.wikipedia.org/wiki/Transposition_cipher#Rail_Fence_cipher)

