# Logs, Logs, Logs!

Welcome to Logs, Logs, Logs! on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Enums

An _enum type_ is a special data type that enables for a variable to be a set of predefined constants.
The variable must be equal to one of the values that have been predefined for it.
Common examples include compass directions (values of `NORTH`, `SOUTH`, `EAST`, and `WEST`) and the days of the week.

Because they are constants, the names of an enum type's fields are in uppercase letters.

### Defining an enum type

In the Java programming language, you define an enum type by using the `enum` keyword.
For example, you would specify a days-of-the-week enum type as:

```java
public enum DayOfWeek {
    SUNDAY,
    MONDAY,
    TUESDAY,
    WEDNESDAY,
    THURSDAY,
    FRIDAY,
    SATURDAY
}
```

You should use enum types any time you need to represent a fixed set of constants.
That includes natural enum types such as the planets in our solar system and data sets where you know all possible values at compile time - for example, the choices on a menu, command line flags, and so on.

### Using an enum type

Here is some code that shows you how to use the `DayOfWeek` enum defined above:

```java
public class Shop {
    public String getOpeningHours(DayOfWeek dayOfWeek) {
        switch (dayOfWeek) {
            case MONDAY:
            case TUESDAY:
            case WEDNESDAY:
            case THURSDAY:
            case FRIDAY:
                return "9am - 5pm";
            case SATURDAY:
                return "10am - 4pm"
            case SUNDAY:
                return "Closed.";
        }
    }
}
```

```java
var shop = new Shop();
shop.getOpeningHours(DayOfWeek.WEDNESDAY);
// => "9am - 5pm"
```

### Adding methods and fields

Java programming language enum types are much more powerful than their counterparts in other languages.
The `enum` declaration defines a _class_ (called an _enum type_).
The enum class body can include methods and other fields:

```java
public enum Rating {
    GREAT(5),
    GOOD(4),
    OK(3),
    BAD(2),
    TERRIBLE(1);

    private final int numberOfStars;

    Rating(int numberOfStars) {
        this.numberOfStars = numberOfStars;
    }

    public int getNumberOfStars() {
        return this.numberOfStars;
    }
}
```

Calling the `getNumberOfStars` method on a member of the `Rating` enum type:

```java
Rating.GOOD.getNumberOfStars();
// => 4
```

## Instructions

In this exercise you'll be processing log-lines.

Each log line is a string formatted as follows: `"[<LVL>]: <MESSAGE>"`.

These are the different log levels:

- `TRC` (trace)
- `DBG` (debug)
- `INF` (info)
- `WRN` (warning)
- `ERR` (error)
- `FTL` (fatal)

You have three tasks.

## 1. Parse log level

Define a `LogLevel` enum that has six elements corresponding to the above log levels.

- `TRACE`
- `DEBUG`
- `INFO`
- `WARNING`
- `ERROR`
- `FATAL`

Next, implement the `LogLine.getLogLevel()` method that returns the parsed the log level of a log line:

```java
var logLine = new LogLine("[INF]: File deleted");
logLine.getLogLevel();
// => LogLevel.INFO
```

## 2. Support unknown log level

Unfortunately, occasionally some log lines have an unknown log level.
To gracefully handle these log lines, add an `UNKNOWN` element to the `LogLevel` enum which should be returned when parsing an unknown log level:

```java
var logLine = new LogLine("[XYZ]: Overly specific, out of context message");
logLine.getLogLevel();
// => LogLevel.UNKNOWN
```

## 3. Convert log line to short format

The log level of a log line is quite verbose.
To reduce the disk space needed to store the log lines, a short format is developed: `"[<ENCODED_LEVEL>]:<MESSAGE>"`.

The encoded log level is a simple mapping of a log level to a number:

- `UNKNOWN` - `0`
- `TRACE` - `1`
- `DEBUG` - `2`
- `INFO` - `4`
- `WARNING` - `5`
- `ERROR` - `6`
- `FATAL` - `42`

Implement the `LogLine.getOutputForShortLog()` method that can output the shortened log line format:

```java
var logLine = new LogLine("[ERR]: Stack Overflow");
logLine.getOutputForShortLog();
// => "6:Stack overflow"
```

## Source

### Created by

- @sanderploegsma