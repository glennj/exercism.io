# Booking Up For Beauty

Welcome to Booking Up For Beauty on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Date-Time

The `java.time` package introduced in Java 8 contains several classes to work with dates and time.

### `LocalDate`

The `java.time.LocalDate` class represents a date without a time-zone in the [ISO-8601 calendar system][iso-8601], such as `2007-12-03`:

```java
LocalDate date = LocalDate.of(2007, 12, 3);
```

Dates can be compared to other dates:

```java
LocalDate date1 = LocalDate.of(2007, 12, 3);
LocalDate date2 = LocalDate.of(2007, 12, 4);

date1.isBefore(date2);
// => true

date1.isAfter(date2);
// => false
```

A `LocalDate` instance has getters to retrieve time portions from it:

```java
LocalDate date = LocalDate.of(2007, 12, 3);

date.getDayOfMonth();
// => 3
```

A `LocalDate` instance has methods to add time units to it:

```exercism/note
These methods return a _new_ `LocalDate` instance and do not update the existing instance, as the `LocalDate` class is immutable.
```

```java
LocalDate date = LocalDate.of(2007, 12, 3);

date.plusDays(3);
// => 2007-12-06
```

### `LocalDateTime`

The `java.time.LocalDateTime` class represents a date-time without a time-zone in the [ISO-8601 calendar system][iso-8601], such as `2007-12-03T10:15:30`:

```java
LocalDateTime datetime = LocalDateTime.of(2007, 12, 3, 10, 15, 30);
```

You can convert a `LocalDate` instance into a `LocalDateTime`:

```java
LocalDate date = LocalDate.of(2007, 12, 3);
LocalDateTime datetime = date.atTime(10, 15, 30);
datetime.toString();
// => "2007-12-03T10:15:30"
```

### Formatting datetimes

Both `LocalDate` and `LocalDateTime` use the [ISO-8601][iso-8601] standard notation when converting from and to a `String`.

```java
LocalDateTime datetime = LocalDateTime.of(2007, 12, 3, 10, 15, 30);
LocalDateTime parsed = LocalDateTime.parse("2007-12-03T10:15:30");

datetime.isEqual(parsed);
// => true
```

Attempting to parse a `LocalDate` or `LocalDateTime` from a `String` like this using a different format is not possible.
Instead, to format dates using a custom format, you should use the `java.time.format.DateTimeFormatter`:

```java
DateTimeFormatter parser = DateTimeFormatter.ofPattern("dd/MM/yyyy");
LocalDate date = LocalDate.parse("03/12/2007", parser);

DateTimeFormatter printer = DateTimeFormatter.ofPattern("MMMM d, yyyy");
printer.format(date);
// => "December 3, 2007"
```

A locale can also be specified when creating the custom format to format and parse for different regions:

```java
DateTimeFormatter.ofPattern("MMMM d, yyyy", Locale.FRENCH).format(date);
// => décembre 3, 2007

DateTimeFormatter.ofPattern("MMMM d, yyyy", Locale.of("pt")).format(date);
// => dezembro 3, 2007
```

[iso-8601]: https://en.wikipedia.org/wiki/ISO_8601

## Instructions

In this exercise you'll be working on an appointment scheduler for a beauty salon in New York that opened on September 15th in 2012.

## 1. Parse appointment date

Implement the `AppointmentScheduler.schedule()` method to parse a textual representation of an appointment date into the corresponding `LocalDateTime`:

```java
AppointmentScheduler scheduler = new AppointmentScheduler();
scheduler.schedule("7/25/2019 13:45:00");
// => LocalDateTime.of(2019, 7, 25, 13, 45, 0)
```

## 2. Check if an appointment has already passed

Implement the `AppointmentScheduler.hasPassed()` method that takes an appointment date and checks if the appointment was somewhere in the past:

```java
AppointmentScheduler scheduler = new AppointmentScheduler();
scheduler.hasPassed(LocalDateTime.of(1999, 12, 31, 9, 0, 0));
// => true
```

## 3. Check if appointment is in the afternoon

Implement the `AppointmentScheduler.isAfternoonAppointment()` method that takes an appointment date and checks if the appointment is in the afternoon (>= 12:00 and < 18:00):

```java
AppointmentScheduler scheduler = new AppointmentScheduler();
scheduler.isAfternoonAppointment(LocalDateTime.of(2019, 03, 29, 15, 0, 0))
// => true
```

## 4. Describe the time and date of the appointment

Implement the `AppointmentScheduler.getDescription()` method that takes an appointment date and returns a description of that date and time:

```java
AppointmentScheduler scheduler = new AppointmentScheduler();
scheduler.getDescription(LocalDateTime.of(2019, 03, 29, 15, 0, 0))
// => "You have an appointment on Friday, March 29, 2019, at 3:00 PM."
```

## 5. Return the anniversary date

Implement the `AppointmentScheduler.getAnniversaryDate()` method that returns this year's anniversary date, which is September 15th:

```java
AppointmentScheduler scheduler = new AppointmentScheduler();
scheduler.getAnniversaryDate()
// => LocalDate.of(<current year>, 9, 15)
```

## Source

### Created by

- @sanderploegsma