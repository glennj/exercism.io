# Error Handling

Welcome to Error Handling on Exercism's Bash Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Implement various kinds of error handling and resource management.

An important point of programming is how to handle errors and close resources even if errors occur.

This exercise requires you to handle various errors.
Because error handling is rather programming language specific you'll have to refer to the tests for your track to see what's exactly required.

## Bash-specific instructions

The goal of this exercise is to consider the number of arguments passed to your program.

Note that you can pass empty strings as arguments.

`./program ""`

This is different from passing no arguments at all.

`./program`

If your program is run with exactly one argument (even if it is an empty string), treat it as a person's name and print a greeting message.
If it is run with zero arguments or more than one argument, print an error message and exit with a non-zero status.

## Source

### Created by

- @jaggededgedjustice

### Contributed to by

- @bkhl
- @budmc29
- @glennj
- @guygastineau
- @IsaacG
- @kotp
- @kytrinyx
- @sbonds
- @sjwarner-bp
- @Smarticles101
- @ZapAnton