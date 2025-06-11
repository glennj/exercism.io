# Kuroko's Clock

Welcome to Kuroko's Clock on Exercism's Elm Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Time

The `Time` module is used to display human-readable dates / times.
While it's technically possibly to manipulate times with `Time`, there are Elm community packages that are far more suited to the task, such as [`justinmimbs/date`][date] or [`CoderDennis/elm-time-format`][elm-time-format].

### Posix

Times are represented by the opaque type `Posix`, which wraps Unix epoch time, the number of milliseconds passed since January 1st, 1970 at midnight UTC.
A `Posix` is an non-ambiguous value that does not vary between time zones, therefore a good choice for keeping time.
In an Elm application, `Posix` times will typically be provided by external sources, such as the browser.

It is possible to transform a `Posix` into a number of millisecond and vice-versa with the following functions:

```elm
posixToMillis : Posix -> Int
millisToPosix : Int -> Posix
```

### Time Zones

In our day-to-day lives, we humans usually don't read `Posix` and instead prefer local time, which depends on our location on Earth, and local daylight-saving rules.
In Elm, this information is encoded in the opaque type `Zone`, usually provided by the browser.

There is a special `Zone` that is always accessible:

```elm
utc : Zone
```

### Human times

With a `Posix` and a `Zone`, we have enough information to show local time in any level of details.
Here is a list of the functions that can be used for that:

```elm
toYear : Zone -> Posix -> Int
toMonth : Zone -> Posix -> Month
toDay : Zone -> Posix -> Int
toWeekday : Zone -> Posix -> Weekday
toHour : Zone -> Posix -> Int
toMinute : Zone -> Posix -> Int
toSecond : Zone -> Posix -> Int
toMillis : Zone -> Posix -> Int
```

You will have spotted two more `Time` custom types:

```elm
type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
type Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
```

It is left to users to convert these custom types to human-readable values.

[date]: https://package.elm-lang.org/packages/justinmimbs/date/latest
[elm-time-format]: https://package.elm-lang.org/packages/CoderDennis/elm-time-format/latest

## Instructions

Your Japanese friend Kuroko is a streamer with an international appeal.
Kuroko's streaming schedule is pretty intricate, each stream seemingly starts at a different time every day.

He asks for your help in building a small application that will display his schedule in his viewers' local time zones: Kuroko's Clock.
He has an additional requirement: the app should be able to display the time in two different languages: Japanese and US English.

## 1. It's a date

Define a function `showLocalDate : Locale -> Int -> Month -> Int -> String` which will display a (valid) local date.

In the `US` locale, the format will be `Month/Day/Year` without leading zeroes.

In the `JP` locale, the format will be `Year年Month月Day日`, also without leading zeroes.

```elm
showLocalDate US 2025 May 1
    -- "5/1/2025"
showLocalDate JP 2025 May 1
    -- "2025年5月1日"
```

## 2. It's show time

Define a function `showLocalTime : Locale -> Int -> Int -> String` which will display a (valid) local time.

In the `US` locale, the hours will be shown in 12-hour notation along with "PM" or "AM", and the minutes will be shown with leading zeros.

In the `JP` locale, the hour will be shown in 24-hour notation before "時" and the minutes will be shown without leading zeros before "分".

```elm
showLocalTime US 13 8
    -- "1:08 PM"
showLocalTime JP 13 8
    -- "13時8分"
```

## 3. Get in the zone

Define a function `showDateTime : Locale -> Zone -> Posix -> String` which will display a date and time relative to a time zone in a specific locale.

Use `showLocalDate` and `showLocalTime` to display the final result.

```elm
showDateTime US Time.utc (Time.millisToPosix 0)
    -- "1/1/1970 12:00 AM"
showDateTime JP Time.utc (Time.millisToPosix 0)
    -- "1970年1月1日0時0分"
```

## Source

### Created by

- @jiegillet