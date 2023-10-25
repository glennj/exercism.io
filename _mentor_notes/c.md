# C notes

- Courtesy @siebenschlaefer

    https://exercism.org/tracks/c/exercises/resistor-color/mentor_discussions/471ce07864e648f4858d2e825b11d6f2

    `const` binds to the left, except if there's not left, then it binds to the right.
    Let me demonstrate that with a pointer:

    ```c
    int * ptr               // a modifiable pointer to a modifiable object

    const int * ptr         // a modifiable pointer to a read-only object
    int const * ptr         // same as const int *

    int * const ptr         // a read-only pointer to a modifiable object

    const int * const ptr   // a read-only pointer to a read-only object
    int const * const ptr   // same as const int * const
    ```

