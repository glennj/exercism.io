# Vehicle Purchase

Welcome to Vehicle Purchase on Exercism's AWK Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## More about Patterns

Recall from the Fundamentals concept, an AWK program is composed of **pattern-action pairs**.

```awk
pattern1 { action1 }
pattern2 { action2 }
...
```

### What do we mean by "pattern"?

The "pattern" is any AWK expression.
The truthiness of the expression's result determines if the action is executed.

### The Empty Pattern

The pattern can be omitted.
In this case, the action is performed for every record.

We can print all the usernames in the passwd file.

```sh
awk -F: '{print $1}' /etc/passwd
```

### Regular Expressions

AWK can compare strings against regular expressions to obtain a boolean result.

Use the `~` regex-matching operator to match a particular field. 
This operator takes a string as the left-hand operand and a regular expression as the right-hand operand.
A regular expression literal is enclosed in `/` slashes.

To find the users in the passwd file who log in with bash:

```sh
awk -F: '$7 ~ /bash/ {print $1}' /etc/passwd
```

`!~` is the "regex does **not** match" operator.

To match a regex against the current record, you can do `$0 ~ /regex/`.
This is so common there is a shorthand: you can drop the `$0` and `~` and simply write `/regex/`

```sh
awk '/regex/' data.txt
```

~~~~exercism/note
Compare that AWK one-liner with the equivalent grep command

```sh
grep 'regex' data.txt
```

AWK gives you a whole programming language without sacrificing succinctness.
~~~~

We'll get more into GNU AWK's regular expression flavor in a further concept.

### Expressions

AWK expressions (arithmetic, logical, or otherwise) can be used as patterns.

To extract all users with UID 1000 or above:

```sh
awk -F: '$3 >= 1000' /etc/passwd
```

Recall that AWK's false values are the number zero and the empty string, and all other numbers or strings are true.
Any expression that evaluates to a number or a string can be used as a pattern.

### Functions

Any [builtin][builtins] or [user-defined][] function can be used in an expression, and hence in the pattern.
A couple of examples:

```awk
length($1) {print "first field is not empty"}
```
```awk
toupper(substr($1, 1, 1)) ~ /[AEIOU]/ {print "starts with a vowel"}
```

### Constant Patterns

A common AWK idiom is:

```sh
{
    xyz()   # some code that transforms each record
}
1
```

`1` is a truthy pattern with no associated action.
This means "print the current record."

[builtins]: https://www.gnu.org/software/gawk/manual/html_node/Built_002din.html
[user-defined]: https://www.gnu.org/software/gawk/manual/html_node/User_002ddefined.html

## Instructions

In this exercise, you will write a program to help you prepare to buy a vehicle.

You have two tasks: determine if you will need to get a license, and estimate the acceptable price for a used vehicle.

## 1. Determine if you will need a drivers license

Some kinds of vehicles require a drivers license to operate them.
Assume only vehicles containing `car` or `truck` require a license; everything else can be operated without a license.

Implement a `pattern {action}` pair that matches when the first field is `needs_license`.
It should get the kind of vehicle from the second field, and print "true" if you need a license for that kind of vehicle.
If you don't need a license, you don't need to print anything.

```sh
echo "needs_license,dumptruck" | gawk -f vehicle-purchase.awk
# => true

echo "needs_license,bike" | gawk -f vehicle-purchase.awk
# => (empty output)
```

## 2. Calculate an estimation for the price of a used vehicle

Now that you have made your decision, you want to make sure you get a fair price at the dealership.
Since you are interested in buying a used vehicle, the price depends on how old the vehicle is.
For a rough estimate, assume if the vehicle is less than 3 years old, it costs 80% of the original price it had when new.
If it is more than 10 years old, it costs 50%.
If the vehicle is at least 3 years old but not older than 10 years, it costs 70% of the original price.

Implement a `pattern {action}` pair that matches when the first field is `resell_price`.
It should get the original price of the vehicle from the second field, and the age from the third field.
The action should print the estimated price.

```sh
echo "resell_price,1000,1" | gawk -f vehicle-purchase.awk
# => 800

echo "resell_price,1000,5" | gawk -f vehicle-purchase.awk
# => 700

echo "resell_price,1000,15" | gawk -f vehicle-purchase.awk
# => 500
```

## Source

### Created by

- @glennj

### Contributed to by

- @IsaacG
- @kotp