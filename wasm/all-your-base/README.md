# All Your Base

Welcome to All Your Base on Exercism's WebAssembly Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Convert a number, represented as a sequence of digits in one base, to any other base.

Implement general base conversion. Given a number in base **a**,
represented as a sequence of digits, convert it to base **b**.

## Note

- Try to implement the conversion yourself.
  Do not use something else to perform the conversion for you.

## About [Positional Notation](https://en.wikipedia.org/wiki/Positional_notation)

In positional notation, a number in base **b** can be understood as a linear
combination of powers of **b**.

The number 42, _in base 10_, means:

(4 _ 10^1) + (2 _ 10^0)

The number 101010, _in base 2_, means:

(1 _ 2^5) + (0 _ 2^4) + (1 _ 2^3) + (0 _ 2^2) + (1 _ 2^1) + (0 _ 2^0)

The number 1120, _in base 3_, means:

(1 _ 3^3) + (1 _ 3^2) + (2 _ 3^1) + (0 _ 3^0)

I think you got the idea!

_Yes. Those three numbers above are exactly the same. Congratulations!_

## WebAssembly-specific Notes

The function signature for the WebAssembly export `convert` is as follows:

```wasm
(func (export "convert")
    (param $arrOffset i32)
    (param $arrLength i32)
    (param $inputBase i32)
    (param $outputBase i32)
    (result i32 i32 i32)
)
```

The first two parameters `$arrOffset` and `$arrLength` express the base offset and length of an array of 32-bit signed integers. The length parameter is sized in number of elements in the array, not bytes. Prior to calling this function, the caller writes this array into the WebAssembly linear memory beginning at offset `$arrOffset`. WebAssembly linear memory is always expressed in little-endian. 

Thus the caller would thus encoded the array `[1,2]` as the following eight byte sequence.

```
| 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 |
| ---- arr[0] ----- | ---- arr[1] ----- |
,0x01,0x00,0x00,0x00,0x02,0x00,0x00,0x00,
```

The parameters `$inputBase` and `$outputBase` do not involve linear memory.

The result type is `(i32 i32 i32)`. The first two values are the `offset` and `length` of your output in linear memory. If you so choose, you may overwrite the addresses of linear memory used for the input. The third return value is an i32 status code used for error handling.

If the third return value expresses an error state, the unit tests do not read the first two return values.

## Source

### Created by

- @bushidocodes