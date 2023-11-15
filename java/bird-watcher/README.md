# Bird Watcher

Welcome to Bird Watcher on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

## Arrays

In Java, data structures that can hold zero or more elements are known as _collections_.
An **array** is a collection that has a fixed size and whose elements must all be of the same type.
Elements can be assigned to an array or retrieved from it using an index.
Java arrays use zero-based indexing: the first element's index is 0, the second element's index is 1, etc.

Here is the standard syntax for initializing an array:

```java
type[] variableName = new type[size];
```

The `type` is the type of elements in the array which may be a primitive type (e.g. `int`) or a class (e.g. `String`).

The `size` is the number of elements this array will hold (which cannot be changed later).
After array creation, the elements are initialized to their default values (typically `0`, `false` or `null`).

```java
// Declare array with explicit size (size is 2)
int[] twoInts = new int[2];
```

Arrays can also be defined using a shortcut notation that allows you to both create the array and set its value:

```java
// Two equivalent ways to declare and initialize an array (size is 3)
int[] threeIntsV1 = new int[] { 4, 9, 7 };
int[] threeIntsV2 = { 4, 9, 7 };
```

As the compiler can now tell how many elements the array will have, the length can be omitted.

Array elements may be assigned and accessed using a bracketed index notation:

```java
// Assign second element by index
twoInts[1] = 8;

// Retrieve the second element by index and assign to the int element
int secondElement = twoInts[1];
```

Accessing an index that is outside of the valid indexes for the array results in an `IndexOutOfBoundsException`.

Arrays can be manipulated by either calling an array instance's methods or properties, or by using the static methods defined in the `Arrays` class (typically only used in generic code).
The most commonly used property for arrays is its length which can be accessed like this:

```java
int arrayLength = someArray.length;
```

Java also provides a helpful utility class [`java.util.Arrays`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Arrays.html) that has lots of useful array-related methods (eg. `Arrays.equals`).

Java also supports [multi-dimensional arrays](https://www.programiz.com/java-programming/multidimensional-array) like `int[][] arr = new int[3][4];` which can be very useful.

The fact that an array is also a _collection_ means that, besides accessing values by index, you can iterate over _all_ its values using a `for-each` loop:

```java
char[] vowels = { 'a', 'e', 'i', 'o', 'u' };

for(char vowel: vowels) {
    // Output the vowel
    System.out.print(vowel);
}

// => aeiou
```

If you want more control over which values to iterate over, a `for` loop can be used:

```java
char[] vowels = { 'a', 'e', 'i', 'o', 'u' };

for (int i = 0; i < 3; i++) {
    // Output the vowel
    System.out.print(vowels[i]);
}

// => aei
```

## Instructions

You're an avid bird watcher that keeps track of how many birds have visited your garden in the last seven days.

You have six tasks, all dealing with the numbers of birds that visited your garden.

## 1. Check what the counts were last week

For comparison purposes, you always keep a copy of last week's counts nearby, which were: 0, 2, 5, 3, 7, 8 and 4. Implement the `BirdWatcher.getLastWeek()` method that returns last week's counts:

```java
BirdWatcher.getLastWeek();
// => [0, 2, 5, 3, 7, 8, 4]
```

## 2. Check how many birds visited today

Implement the `BirdWatcher.getToday()` method to return how many birds visited your garden today. The bird counts are ordered by day, with the first element being the count of the oldest day, and the last element being today's count.

```java
int[] birdsPerDay = { 2, 5, 0, 7, 4, 1 };
BirdWatcher birdCount = new BirdWatcher(birdsPerDay);
birdCount.getToday();
// => 1
```

## 3. Increment today's count

Implement the `BirdWatcher.incrementTodaysCount()` method to increment today's count:

```java
int[] birdsPerDay = { 2, 5, 0, 7, 4, 1 };
BirdWatcher birdCount = new BirdWatcher(birdsPerDay);
birdCount.incrementTodaysCount();
birdCount.getToday();
// => 2
```

## 4. Check if there was a day with no visiting birds

Implement the `BirdWatcher.hasDayWithoutBirds()` method that returns `true` if there was a day at which zero birds visited the garden; otherwise, return `false`:

```java
int[] birdsPerDay = { 2, 5, 0, 7, 4, 1 };
BirdWatcher birdCount = new BirdWatcher(birdsPerDay);
birdCount.hasDayWithoutBirds();
// => true
```

## 5. Calculate the number of visiting birds for the first number of days

Implement the `BirdWatcher.getCountForFirstDays()` method that returns the number of birds that have visited your garden from the start of the week, but limit the count to the specified number of days from the start of the week.

```java
int[] birdsPerDay = { 2, 5, 0, 7, 4, 1 };
BirdWatcher birdCount = new BirdWatcher(birdsPerDay);
birdCount.getCountForFirstDays(4);
// => 14
```

## 6. Calculate the number of busy days

Some days are busier that others. A busy day is one where five or more birds have visited your garden.
Implement the `BirdWatcher.getBusyDays()` method to return the number of busy days:

```java
int[] birdsPerDay = { 2, 5, 0, 7, 4, 1 };
BirdWatcher birdCount = new BirdWatcher(birdsPerDay);
birdCount.getBusyDays();
// => 2
```

## Source

### Created by

- @samuelteixeiras
- @ystromm