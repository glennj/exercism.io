# Secrets

Welcome to Secrets on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Bitwise Operations

Bitwise operations allow us to manipulate individual digits within binary numbers.

Elm provides several bitwise operators in its [Bitwise module](https://package.elm-lang.org/packages/elm/core/latest/Bitwise)

### Basic operations

Modifying individual bits of a number is called _masking_.
A _mask_ is a number where specific bits have been set in a particular way to manipulate another number using bitwise operators such as `and`, `or`, and `xor`.

#### and

`and` combines two numbers by keeping only the bits that are `1` in both.
This is useful for checking to see if an individual bit is set.
For example, to check if the 4th bit of a number is set to `1`, `and` it with a mask of `01000` (`8` in decimal) and see if the result is non-zero:

```elm
Bitwise.and 13 8 --> 8
--  13 = 01101
--   8 = 01000
-- and = 01000 = 8
```

#### or

`or` combines two numbers by setting each bit to `1` if it is `1` in either or both numbers.
This is useful for setting a specific bit to `1`.
For example, to set the 2nd bit in `10101`, `or` it with the mask `00010`:

```elm
Bitwise.or 21 2 --> 23
-- 21 = 10101
--  2 = 00010
-- or = 10111 = 23
```

#### Exclusive-or (xor)

`xor` combines two numbers by setting each bit to `1` if it is `1` in one number but `0` in the other.
This is useful for flipping a bit to its opposite value:

```elm
Bitwise.xor 20 5 --> 17
--  20 = 10100
--   5 = 00101
-- xor = 10001 = 17
```

#### Complement

`complement` inverts each bit of a number (`0` becomes `1`, `1` becomes `0`).

Note that this will result in positive numbers becoming negative, and negative numbers becoming positive.
This is because negative numbers in binary are represented with `1` in the left-most position.

```elm
Bitwise.complement 21 --> -22
--         21 = 00000000000000000000000000010101
-- complement = 11111111111111111111111111101010 = -22
```

#### Bit shifting

The following operators move bits left or right by a specified number of positions, effectively multiplying or dividing by powers of 2.

`shiftLeftBy` moves bits to the left, filling in with `0` from the right-hand side.
For example, to shift `21` left by 3 places:

```elm
Bitwise.shiftLeftBy 3 21 --> 168
--  21 = 10101
-- shiftLeftBy 3 = 10101000 = 168
```

This is the same as saying `21 * 2^3 = 21 * 2 * 2 * 2 = 168`

`shiftRightBy`: Moves bits to the right:

```elm
Bitwise.shiftRightBy 2 21 --> 5
--  21 = 10101
-- shiftRightBy 2 = 00101 = 5
```

Shifting to the right by 2 places is the same as integer division by 4.

Note that this function duplicates whatever value is in the leftmost bit.
So, negative numbers will stay negative:

```elm
Bitwise.shiftRightBy 3 -21 --> -3
--  -21 = 111...101011
-- shiftRightBy 3 = 111...11101 = -3
```

If you want to shift right and fill in with zeros, use `shiftRightZfBy`:

```elm
Bitwise.shiftRightZfBy 3 -21 --> 536870909
--  -21 = 111...101011
-- shiftRightZfBy 3 = 00111...11101 = 536870909
```

## Instructions

Your friend has just sent you a message with an important secret.
Not wanting to make it easy for others to read it, the message was encrypted by performing a series of bit manipulations.
You will need to write the functions to help decrypt the message.

## 1. Shift back the bits

The first step in decrypting the message is to undo the shifting from the encryption process by shifting the bits back to the right.
There will be further steps in the decryption process that assume `0`s are inserted from the left hand side.

Implement the `shiftBack` function that takes a number of places to shift by and a value and performs the shift.

```elm
shiftBack 2 42 --> 10
```

## 2. Set some bits

Next, there are some bits that need to be set to `1`.

Implement the `setBits` function that takes a mask and value and returns the result of setting the bits in value to `1`.
A bit from value should be set to `1` where the bit in the mask is also `1`.
All other bits should be kept unchanged.

```elm
setBits 66 212 --> 64
```

## 3. Flip specific bits

Some bits are flipped during encryption.
They will need to be flipped back to decrypt the message.

Implement the `flipBits` function that takes a mask and a value.
The mask indicates which bits in the value to flip.
If the bit is `1` in mask, the bit is flipped in the value.
All other bits are kept unchanged.

```elm
flipBits 23 157 --> 138
```

## 4. Clear specific bits

There are also certain bits that always decrypt to 0.

Implement the `clearBits` function that takes a mask and a value.
The bits in the `value` should be set to 0 where the bit in the mask is 1.
All other bits should be kept unchanged.

```elm
clearBits 2 15 --> 13
```

## 5. Decrypt a message

Now that you have all the functions you need, you can decode your friend's message.
Implement the `decrypt` function that performs the following operations:

1. Set the bits from the year your friend was born (1996)
2. Flip the result with the year that you first met (2009)
3. Shift the bits back by the number of classes you take together (5)
4. Clear the first and fifth bit.

```elm
decrypt 380182 --> 11840
```

Please implement `decrypt` with all the functions you implemented in the previous tasks.

## Source

### Created by

- @stewartmurrie