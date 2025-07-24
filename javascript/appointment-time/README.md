# Appointment Time

Welcome to Appointment Time on Exercism's JavaScript Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

JavaScript has a built-in object `Date` which stores date and time, and provides methods for their management.

<!-- prettier-ignore -->
~~~exercism/caution
It was based on Java's `java.util.Date` class, which was replaced in the early 2010s, but for backwards compatibility, JavaScript's `Date` sticks around.

Because of how hard it is to work with Dates in general and because of how bad or non-existing timezone handling is, many libraries exist such as `moment.js`, `day.js`, `date-fns` and `luxon`.
None of these are available on Exercism.

In your own projects, do not use a deprecated / unmaintained package such as `moment.js` but rely on more modern alternatives like `luxon`, or the not yet widely available [Temporal][mdn-temporal].
This exercise focusses on `Date`, which will remain relevant until the end of JavaScript.

[mdn-temporal]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal
~~~

## Creation

A `Date` object is an instance of the `Date` class.
It can be created without passing any arguments to the constructor function.
This results in a `Date` object that represents the current date and time:

```javascript
const now = new Date();
// => Thu Apr 14 2022 11:46:08 GMT+0530 (India Standard Time)
// Shows current day, date and time (in your time zone).
```

### **Unix timestamp (number)**

If a number is passed in, this will be interpreted as a `timestamp`.
A timestamp is an integer number representing the number of **milliseconds** that has passed since **1 January 1970 [UTC][defn-utc]+0**.

```javascript
const epoch = new Date(0);
// Thu Jan 01 1970 01:00:00 GMT+0100 (Central European Standard Time)

const another = new Date(1749508766627);
// Tue Jun 10 2025 00:39:26 GMT+0200 (Central European Summer Time)
```

One may expect `new Date(0)` to generate the "earliest" date object, but JavaScript will convert the date to your local timezone, which means that only those around [GMT / with an UTC+0][defn-gmt] timezone will actually get the [Unix epoch][defn-unix-epoch] value.

### **ISO 8601 timestamp (string)**

You can pass a string value representing a date to the `Date` constructor.
The **only** format that is consistent across implementations is the [simplified version][mdn-date-string-format] of the internationally recognized and standardized so-called [ISO 8601 timestamp strings][defn-iso8601].

A moment in time at [UTC][defn-gmt] looks like this:

```text
YYYY-MM-DDTHH:mm:ss.mssZ
YYYYMMDDTHHmmss.mssZ
```

Where the following substitutions take place:

| Key  | Description                                   | Default |
| ---- | --------------------------------------------- | ------- |
| YYYY | The calendar year, represented in 4 digits    |         |
| MM   | The calendar month, represented in 2 digits   | 01      |
| DD   | The calendar day, represented in 2 digits     | 01      |
| T    | A literal letter T, separating date from time |         |
| HH   | The hours in a 24-hour clock, 2 digits        | 00      |
| mm   | The minutes, 2 digits                         | 00      |
| ss   | The seconds, 2 digits                         | 00      |
| mss  | The milliseconds, 3 digits                    | 000     |
| Z    | A literal letter Z, or an offset `+/-HH:mm`   |         |

The literal letter `Z` indicates UTC (no timezone, no Day Light Savings).

Because there are default values for most components, leaving parts off at the end is valid:

```text
YYYY-MM-DD
```

Defaults to a time of 00:00:00.000

If the timestamp does not end in `Z`, and it does not end with `+HH:mm` or `-HH:mm` (indicating a timezone offset), because of historical reasons, the following applies:

> When the time zone offset is absent, date-only forms are interpreted as a UTC time and date-time forms are interpreted as a local time.
> The interpretation as a UTC time is due to a historical spec error that was not consistent with ISO 8601 but could not be changed due to web compatibility.
> See [Broken Parser – A Web Reality Issue][ref-broken-parser].

<!-- prettier-ignore -->
~~~exercism/caution
Other formats that are accepted by `Date.parse` may or may not work.
When working with Dates in JavaScript, _always_ use an ISO 8601 timestamp when converting from a `string` to a `Date`.

Date-only forms are allowed, but not all ISO 8601 formats are supported.
Consult the [simplified version explanation page on MDN][mdn-date-string-format].

[mdn-date-string-format]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#date_time_string_format
~~~

### **Date object**

An existing date object can also be used as a constructor argument.
This makes a copy of the existing `Date` object with the same date and time.

```javascript
const t1 = new Date();
const t2 = new Date(t1);
// Values of t1 and t2 will be the same.
```

### **Supplying individual date and time component values**

A date representing a date can be created by passing three numbers.
A date representing a date and time can be created by passing in 6 numbers.

```javascript
const date1 = new Date(95, 11, 17);
// Creates Date for Dec 17 1995 00:00 if your local timezone is equivalent to UTC.

const date2 = new Date(2013, 12, 5, 13, 24, 0);
// Creates Date for Jan 5 2014 13:24 if your local timezone is equivalent to UTC.
```

The second value is the `month`, which starts at `0` for January, up to `11` for December.

## `Date.parse()`

You may find mentions of or references to a date parsing function `Date.parse`.
Because there are only a few invariants (truths) for this function and because the rest of the implementation is not specified (and thus not standard), one should not use it.

## Accessing `Date` components

There are various methods on date objects that return the components of the date:

```javascript
getFullYear(); // Get the year (4 digits)
getMonth(); // Get the month, from 0 to 11.
getDate(); // Get the day of month, from 1 to 31.
getHours(); // Get the hour in a 24 clock, from 0 to 23
getMinutes(); // Get the minutes, from 0 to 59
getSeconds(); // Get the seconds, from 0 to 59
getMilliseconds(); // Get the milliseconds, from 0 and 999
getDay(); // Get the day of week, from 0 (Sunday) to 6 (Saturday).
```

Each of these has an applicable `set` variant (e.g. `setFullYear`) to update the value.
Any overflowing value rolls over to the next component:

```javascript
const date = new Date('2025-02-28T12:42:00Z');
// => Fri Feb 28 2025 13:42:00 GMT+0100 (Central European Standard Time)

date.setDate(29);
// there was no February 29th in 2025.

date.getDate();
// => 1

date.toString();
// => Sat Mar 01 2025 13:42:00 GMT+0100 (Central European Standard Time)
```

There are UTC variants for all the methods that disregard the local timezone.

## Converting from date

Date objects have a method `getTime()` that returns the UNIX timestamp in milliseconds, ie. amount of milliseconds the midnight at the beginning of January 1, 1970, UTC.
Additionally, a method `toISOString()` is available to convert from a date object to a ISO 8601 timestamp string.

## Comparing Dates

Greater than (`>`) and greater than or equals (`>=`) as well as less than (`<`) and less than or equals (`<=`) can be used directly between two dates or a date and a number.
This works because JavaScript will try to coerce the date to a primitive.

<!-- prettier-ignore -->
~~~exercism/advanced
When doing a comparison between two dates or date and a number, JavaScript calls [`[Symbol.toPrimitive]("number")`][mdn-to-primitive] which internally calls [`date.valueOf()`][mdn-date-value-of].
The latter is the same as calling [`date.getTime()`][mdn-date-get-time].

If you do not want to rely on this behaviour, convert to a number using `getTime()` first.

[mdn-to-primitive]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Symbol.toPrimitive
[mdn-date-value-of]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/valueOf
[mdn-date-get-time]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime
~~~

Dates cannot be compared using equality (`==`, and `===`), but the result of `.getTime()` can.

[defn-utc]: https://simple.wikipedia.org/wiki/Coordinated_Universal_Time
[defn-gmt]: https://simple.wikipedia.org/wiki/Greenwich_Mean_Time
[defn-unix-epoch]: https://en.wikipedia.org/wiki/Epoch_%28computing%29
[defn-iso8601]: https://en.wikipedia.org/wiki/ISO_8601
[mdn-date-string-format]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#date_time_string_format
[ref-broken-parser]: https://maggiepint.com/2017/04/11/fixing-javascript-date-web-compatibility-and-reality/

## Instructions

In this exercise you will work on some functions in order to manage appointments.
The system stores everything in ISO 8601 formatted strings, but that's not how people use the calendar.
Various functions are necessary to convert between the various formats.

## 1. Create an appointment

Create an appointment `n` days from now at current time.
The function takes `n` days and return the appointment time of `n` days from now.

```javascript
createAppointment(4, now);
// Given now is Sun Oct 05 2022 23:28:43 GMT+0600 (Bangladesh Standard Time)
// => Sun Oct 09 2022 23:28:43 GMT+0600 (Bangladesh Standard Time)
```

If the second parameter `now` is unused, the current time should be used instead.

## 2. Convert a date into a timestamp

Various tools only work with the internationally standardized ISO 8601 format.
Write the function `getAppointmentTimestamp` to take a date and return a string in that format.

```javascript
const appointment = new Date(Date.UTC(2010, 6, 16, 12, 42, 0, 0));

getAppointmentTimestamp(appointment);
// => '2010-07-16T12:42:00.000Z'
```

## 3. Get the details of an appointment

Timestamps are hard to read; a function to get the appointment details should help with that.
The function `getAppointmentDetails` takes a timestamp in the ISO 8601 format, and returns the year, month, date, hour, and minute.

```javascript
getAppointmentDetails('2022-04-24T08:15:00.000');
// => { year: 2022, month: 3, date: 24, hour: 8, minute: 15 }
```

## 4. Update an appointment with the given options

The function will receive first argument as appointment time and second argument of object of some options.
You have to update the appointment according to the options in the object and return the new appointment date.
The options object could have multiple options.

```javascript
updateAppointment('2022-02-09T09:20:00.000', { month: 6 });
// => { year: 2022, month: 6, date: 9, hour: 10, minute: 20 }
```

## 5. Get the available time between two appointments

The function will receive two appointments (timestamps) as arguments.
You have to return the difference between those two times in seconds.

Because half a second is almost meaningless, round the number before returning it.

```javascript
timeBetween('2022-12-12T09:20:00.000', '2022-12-18T08:30:00.000');
// => 515400
```

## 6. Check if an appointment is now valid or not

Finally, when the appointment is made, the system needs to check if it's valid.
In other words, the appointment must be in the future, and not the past.

Write the function `isValid` which takes two arguments, an appointment timestamp (string), and the current time as a timestamp (string) and returns `true` if the appointment is in the future, given the current time.

```javascript
isValid('2022-02-11T23:00:00.000', '2022-02-08T23:00:00.000');
// => true
```

## Source

### Created by

- @SalahuddinAhammed
- @SleeplessByte