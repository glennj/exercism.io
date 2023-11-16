# Calculator Conundrum

Welcome to Calculator Conundrum on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Exceptions

The Java programming language uses _exceptions_ to handle errors and other exceptional events.

### What is an exception

An exception is an event that occurs during the execution of a program that disrupts the normal flow of instructions.
Exceptions are raised explicitly in Java, and the act of raising an exception is called _throwing an exception_.
The act of handling an exception is called _catching an exception_.

Java distinguishes three types of exceptions:

1. Checked exceptions
2. Unchecked exceptions
3. Errors

#### Checked exceptions

_Checked exceptions_ are exceptional conditions that an application should anticipate and recover from.
An example of a checked exception is the `FileNotFoundException` which occurs when a method is trying to read a file that does not exist.

This type of exception is checked at compile-time: methods that throw checked exceptions should specify this in their method signature, and code calling a method that might throw a checked exception is required to handle it or the code will not compile.

All exceptions in Java that do not inherit from `RuntimeException` or `Error` are considered checked exceptions.

#### Unchecked exceptions

_Unchecked exceptions_ are exceptional conditions that an application usually cannot anticipate or recover from.
An example of an unchecked exception is the `NullPointerException` which occurs when a method that is expecting a non-null value but receives `null`.

This type of exception is not checked at compile-time: methods that throw unchecked exceptions are not required to specify this in their method signature, and code calling a method that might throw an unchecked exception is not required to handle it.

All exceptions in Java that inherit from `RuntimeException` are considered unchecked exceptions.

#### Errors

_Errors_ are exceptional conditions that are external to an application.
An example of an error is the `OutOfMemoryError` which occurs when an application is trying to use more memory than is available on the system.

Like unchecked exceptions, errors are not checked at compile-time. They are not usually thrown from application code.

All exceptions in Java that inherit from `Error` are considered errors.

### Throwing exceptions

A method in Java can throw an exception by using the `throw` statement.

#### Throwing a checked exception

When throwing a checked exception from a method, it is required to specify this in the method signature by using the `throws` keyword, as shown in the example below.
This forces calling code to anticipate that an exception might be thrown and handle it accordingly.

```java
public class InsufficientBalanceException extends Exception {

}

public class BankAccount {
  public void withdraw(double amount) throws InsufficientBalanceException {
    if (balance < amount) {
      throw new InsufficientBalanceException();
    }

    // rest of the method implementation
  }
}
```

#### Throwing an unchecked exception

When throwing an unchecked exception from a method, it is not required to specify this in the method signature - although it is supported.

```java
public class BankAccount {
  public void withdraw(double amount) {
    if (amount < 0) {
      throw new IllegalArgumentException("Cannot withdraw a negative amount");
    }

    // rest of the method implementation
  }
}
```

### Handling exceptions

Handling exceptions in Java is done with the `try`, `catch` and `finally` keywords.

- Code statements that might throw an exception should be wrapped in a `try` block.
- The `try` block is followed by one or more `catch` blocks that catch the exceptions thrown in the `try` block.
- The `catch` blocks are optionally followed by a `finally` block that always executes after the `try` block, regardless of whether an exception was thrown or not.

The following example shows how these keywords work:

```java
public class ATM {
  public void withdraw(BankAccount bankAccount, double amount) {
    try {
      System.out.println("Withdrawing " + amount);
      bankAccount.withdraw(amount);
      System.out.println("Withdrawal succeeded");
    } catch (InsufficientBalanceException) {
      System.out.println("Withdrawal failed: insufficient balance");
    } catch (RuntimeException e) {
      System.out.println("Withdrawal failed: " + e.getMessage());
    } finally {
      System.out.println("Current balance: " + bankAccount.getBalance());
    }
  }
}
```

In this example, when no exception is thrown, the following is printed:

```
Withdrawing 10.0
Withdrawal succeeded
Current balance: 5.0
```

However, should the `bankAccount.withdraw(amount)` statement throw an `InsufficientBalanceException`, the following is printed:

```
Withdrawing 10.0
Withdrawal failed: insufficient balance
Current balance: 5.0
```

Or, in case an unchecked exception is thrown by the `bankAccount.withdraw(amount)`, the following is printed:

```
Withdrawing -10.0
Withdrawal failed: Cannot withdraw a negative amount
Current balance: 5.0
```

## Instructions

In this exercise you will be building error handling for a simple integer calculator.
To make matters simple, methods for calculating addition, multiplication and division are provided.

The goal is to have a working calculator that returns a String with the following pattern: `16 + 51 = 67`, when provided with arguments `16`, `51` and `+`.

```java
CalculatorConundrum calculator = new CalculatorConundrum();

calculator.calculate(16, 51, "+");
// => returns "16 + 51 = 67"

calculator.calculate(32, 6, "*");
// => returns "32 * 6 = 192"

calculator.calculate(512, 4, "/");
// => returns "512 / 4 = 128"
```

## 1. Implement the method calculate to support a few basic operations

The main method for implementation in this task will be the `CalculatorConundrum.calculate()` method.
It takes three arguments.
The first two arguments `operand1` and `operand2` are integer numbers on which an operation is going to operate.
The third argument `operation` is of type String and for this exercise it is necessary to implement the following operations:

- addition using the `+` String
- multiplication using the `*` String
- division using the `/` String

## 2. Handle illegal operations

Update the `CalculatorConundrum.calculate()` method to handle illegal operations:

- When the `operation` argument is `null`, an `IllegalArgumentException` should be thrown with the message `Operation cannot be null`.
- When the `operation` argument is `""`, an `IllegalArgumentException` should be thrown with the message `Operation cannot be empty`.
- When the `operation` argument is any operation other than `+`, `*`, or `/`, an `IllegalOperationException` should be thrown with the message `Operation '{operation}' does not exist`.

```java
calculator.calculate(10, 1, null);
// => throws IllegalArgumentException with message "Operation cannot be null"

calculator.calculate(10, 1, "");
// => throws IllegalArgumentException with message "Operation cannot be empty"

calculator.calculate(10, 1, "-");
// => throws IllegalOperationException with message "Operation '-' does not exist"
```

## 3. Handle the exception thrown when dividing by zero

In Java, attempting to divide by zero throws an `ArithmeticException`.
Update the `CalculatorConundrum.calculate()` method to catch this exception and then throw an `IllegalOperationException` with the message `Division by zero is not allowed` and the caught `ArithmeticException` as its cause.

```java
calculator.calculate(512, 0, "/");
// => throws IllegalOperationException with message "Division by zero is not allowed"
```

## Source

### Created by

- @rv02
- @jmrunkle

### Contributed to by

- @sanderploegsma