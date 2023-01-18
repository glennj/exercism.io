# Hints

## General

## 1. Check what the counts were last week

- Return the _second last_ element of the array.

## 2. Check how many birds visited yesterday

- As the data is an array of arrays, you'll need to index once to get the correct weekly array, then index again to get the wanted day.
- "Today" is the last element of the weekly array.

## 3. Calculate the total number of visiting birds

- It's possible to calculate the sum of an array using the [add][manual-add] filter.
  You saw this expression in the Basics concept and the Shopping exercise.

## 4. Calculate the number of busy days

- You'll need to _select_ the days with count greater then or equal to 5, then return the size of the resulting array.

## 5. Check if there was a day with no visiting birds

- The one-argument version of the [any][manual-any] filter will be appropriate.

[manual-add]: https://stedolan.github.io/jq/manual/v1.6/#add
[manual-any]: https://stedolan.github.io/jq/manual/v1.6/#all,all(condition),all(generator;condition)