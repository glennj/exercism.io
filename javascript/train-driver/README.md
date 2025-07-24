# Train Driver

Welcome to Train Driver on Exercism's JavaScript Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

JavaScript has a built-in `...` operator that makes it easier to work with indefinite numbers of elements. Depending on the context, it's called either a _rest operator_ or _spread operator_.

## Rest operator

### Rest elements

When `...` appears on the left-hand side of an assignment, those three dots are known as the `rest` operator. The three dots together with a variable name is called a rest element. It collects zero or more values, and stores them into a single array.

```javascript
const [a, b, ...everythingElse] = [0, 1, 1, 2, 3, 5, 8];
a;
// => 0
b;
// => 1
everythingElse;
// => [1, 2, 3, 5, 8]
```

Note that in JavaScript, unlike some other languages, a `rest` element cannot have a trailing comma. It _must_ be the last element in a destructuring assignment. The example below throws a `SyntaxError`:

```javascript
const [...items, last] = [2, 4, 8, 16]
```

### Rest properties

Similarly to arrays, the rest operator can also be used to collect one or more object properties and store them in a single object.

```javascript
const { street, ...address } = {
  street: 'Platz der Republik 1',
  postalCode: '11011',
  city: 'Berlin',
};
street;
// => 'Platz der Republik 1'
address;
// => {postalCode: '11011', city: 'Berlin'}
```

## Rest parameters

When `...` appears in a function definition next to its last argument, that parameter is called a _rest parameter_. It allows the function to accept an indefinite number of arguments as an array.

```javascript
function concat(...strings) {
  return strings.join(' ');
}
concat('one');
// => 'one'
concat('one', 'two', 'three');
// => 'one two three'
```

## Spread

### Spread elements

When `...` appears on the right-hand side of an assignment, it's known as the `spread` operator. It expands an array into a list of elements. Unlike the rest element, it can appear anywhere in an array literal expression, and there can be more than one.

```javascript
const oneToFive = [1, 2, 3, 4, 5];
const oneToTen = [...oneToFive, 6, 7, 8, 9, 10];
oneToTen;
// => [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
const woow = ['A', ...oneToFive, 'B', 'C', 'D', 'E', ...oneToFive, 42];
woow;
// =>  ["A", 1, 2, 3, 4, 5, "B", "C", "D", "E", 1, 2, 3, 4, 5, 42]
```

### Spread properties

Similarly to arrays, the spread operator can also be used to copy properties from one object to another.

```javascript
let address = {
  postalCode: '11011',
  city: 'Berlin',
};
address = { ...address, country: 'Germany' };
// => {
//   postalCode: '11011',
//   city: 'Berlin',
//   country: 'Germany',
// }
```

## Instructions

Your friend Linus is a train driver who drives cargo trains between cities.
Although they are amazing at handling trains, they are not amazing at handling logistics or computers.
They would like to enlist your programming help organizing train details and correcting mistakes in route data.

```exercism/note
To practice, use the rest or spread operator to solve each of the tasks below.
```

## 1. Create a list of all wagons

Your friend has been keeping track of each wagon identifier (ID), but they are never sure how many wagons the system is going to have to process at any given time.
It would be much easier for the rest of the logistics program to have this data packaged into a unified `array`.

Implement a function `getListOfWagons` that accepts an arbitrary number of wagon IDs which are the IDs of each wagon.
Each ID will be a positive integer.
The function should then return the given IDs as a single `array`.

```javascript
getListOfWagons(1, 7, 12, 3, 14, 8, 5);
// => [1, 7, 12, 3, 14, 8, 5]
```

## 2. Move the first two elements to the end of the array

At this point, you are starting to get a feel for the data and how it's used in the logistics program.
The ID system always assigns the locomotive an ID of **1**, with the remainder of the wagons in the train assigned a randomly chosen ID greater than **1**.

Your friend had to connect two new wagons to the train and forgot to update the system!
Now, the first two wagons in the train `array` have to be moved to the end, or everything will be out of order.

Linus would be really grateful to you for fixing their mistakes.

Implement a function `fixListOfWagons` that accepts an array of the id of each wagon.
It `return`s an `array` where the 2 first elements repositioned to the end of the `array` so that the locomotive can be in the front.

```javascript
eachWagonsID = [2, 5, 1, 7, 4, 12, 6, 3, 13];
fixListOfWagons(eachWagonsID);
// => [1, 7, 4,  12, 6, 3, 13, 2, 5]
```

## 3. Add missing values

Uh-oh. some wagons seem to have gone missing.

Fortunately, your friend just found another `array` which appears to contain the missing wagon IDs, and would like you to add them into the main wagon ID `array`.
All they can remember is that the missing values should be placed directly after the designated locomotive.

Given this new information, write a function called `correctListOfWagons` that takes two arrays which have the IDs of each wagon as the arguments.
The wagon IDs of the second `array` should be added into the first `array` directly after the locomotive (ID 1).

```javascript
eachWagonsID = [1, 5, 20, 7, 4, 8];
missingWagons = [3, 17, 6, 15];
correctListOfWagons(eachWagonsID, missingWagons);
// => [1, 3, 17, 6, 15, 5, 20, 7, 4, 8]
```

## 4. Extend routing information

Now that all the wagon data is correct, your friend would like you to update the systems routing information.
Initial routing information has been constructed as an `object`, and you friend would like you to update it with the additions provided.
Every route requires slightly different information, so your friend would really prefer a generic solution.

Implement a function `extendRouteInformation` that accepts two `objects`.
The first `object` contains which cities the train route moves between.

The second `object` contains other routing details such as train speed or length.
The function should return a consolidated `object` with all routing information.

```exercism/note
The variable `moreRouteInformation` can contain different properties.
```

```javascript
route = { from: 'Berlin', to: 'Hamburg' };
moreRouteInformation = { length: '100', speed: '50' };
extendRouteInformation(route, moreRouteInformation);
// => {from: "Berlin", to: "Hamburg", length: "100", speed: "50"}
```

## 5. Separate arrival time from routing information

Your friend has noticed that they don't need the arrival time in the routing information.
Therefore your friend would like you to separate the arrival time from the routing information.

Implement a function `separateTimeOfArrival` that accepts an object with the routing information.
The function should return an array where the first element of the array is the arrival time and the second element is an object with the routing information without arrival time.

```javascript
routeInformation = {
  from: 'Berlin',
  to: 'Hamburg',
  length: '100',
  timeOfArrival: '10:10',
};
separateTimeOfArrival(routeInformation);
// => ["10:10", {from: "Berlin", to: "Hamburg", length: "100"}]
```

## Source

### Created by

- @meatball133

### Contributed to by

- @bethanyg
- @junedev