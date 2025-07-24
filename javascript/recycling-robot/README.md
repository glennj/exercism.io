# Recycling Robot

Welcome to Recycling Robot on Exercism's JavaScript Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

Knowning what the type of a piece of data is, is often very important for code to run smoothly and without errors.

Javascript has several ways to check the type of a value or object.

```exercism/note
Javascript's type checking mechanisms can be somewhat unreliable.

For better type safety and stronger types, you should probably use TypeScript, a language that builds on JavaScript, but with the type syntax of a static-typed language.
```

## The `typeof` operator

The `typeof` operator returns the type of its operand.
The output is a string matching the name of one of the [primitive data types][primitives], except for `"null"`.
It can also be `"function"` or `"object"`.

```javascript
typeof undefined;
// => "undefined"

typeof true;
// => "boolean"

typeof 42;
// => "number"

typeof 'Hello, World!';
// => "string"

typeof function () {
  return 'Hello, World';
};
// => "function"

typeof [1, 2, 3, 4];
// => "object"

typeof { city: 'Stockholm', country: 'Sweden' };
// => "object"
```

For [historical reasons][`typeof null` is `"object"`].

## The `instanceof` operator

For checking the type of an object, you can use the `instanceof` operator.
It evaluates into a `boolean` depending on whether the second operand is included in the first operands' [prototype chain][prototype chain].
To clarify, `instanceof` will return whether the first operand is an instance of second operand or one of its child classes.
`instanceof` only works on objects.

```javascript
class Beverage {
  // ...
}

// The Coffee class is a child of the Beverage class.
class Coffee extends Beverage {
  // ...
}

const java = new Coffee();

java instanceof Coffee;
// => true

java instanceof Beverage;
// => true
```

````exercism/advanced
The `Array` class has a method called `Array.isArray()` that checks if its argument is an array.

While `instanceof Array` will not work with an array created in a different realm such as an `iframe` in a webpage, `Array.isArray()` will.

This is because the Array class has a different constructor in each realm, and each `iframe` has its own ream, meaning that the function in the prototype chain will be different, causing `instanceof Array` to fail.
`Array.isArray()` is capable of ignoring this, and should always be used when possible.

It can also survive false positives where an object isn't actually an `Array`, and merely has `Array` in its prototype chain.

```javascript
({ __proto__: Array.prototype }) instanceof Array
// => true

Array.isArray({ __proto__: Array.prototype })
// => false
```

````

## The `in` operator

The `in` operator returns whether the first operand is a property of the second operand.
It does not check that the property has a defined value.
A property set to `undefined` will still be detected by `in`.

```javascript
class Coffee {
  constructor() {
    this.temperature = 'hot';
    this.isDarkMatter = undefined;
  }

  coolDown() {
    this.temperature = 'warm';
  }
}

const espresso = new Coffee();

'temperature' in espresso;
// => true

'color' in espresso;
// => false

'isDarkMatter' in espresso;
// => true
```

````exercism/note
`in` will return `true` for inherited properties and methods.

```javascript
"coolDown" in espresso
// => true

"constructor" in espresso
// => true
```

To avoid this, use `Object.hasOwn()` instead
````

## The `Object.hasOwn()` function

The `Object.hasOwn()` method returns whether the specified object _owns the given property_ (it is not inherited or a method).

```javascript
class Coffee {
  constructor() {
    this.temperature = 'hot';
  }

  coolDown() {
    this.temperature = 'warm';
  }
}
const cappuccino = new Coffee();

Object.hasOwn(cappucino, 'temperature');
// => true

Object.hasOwn(cappucino, 'constructor');
// => false

Object.hasOwn(cappucino, 'coolDown');
// => false
```

[primitives]: https://developer.mozilla.org/en-US/docs/Glossary/Primitive
[typeof null is object]: https://2ality.com/2013/10/typeof-null.html
[prototype chain]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Inheritance_and_the_prototype_chain

## Instructions

## Instructions

You have been hired by a recycling center.
Due to lack of space, all the products are put on the same conveyor belt, but this has lead to different materials mixing together, making them unusable.
To fix this, you have been tasked with making functions to identify the type of a product.

### 1. Check if a value is a boolean

Implement the `isBoolean` function, that checks if a value is a boolean.

```javascript
isBoolean(true);
// => true

isBoolean(null);
// => false
```

### 2. Check if a value is a number.

Implement the `isNumber` function, that checks if a value is a _finite_ `number` or `bigint`, ie. not `NaN` or `Infinity`.

Sometimes, the device for reading IDs fails and reads a non-numeric value as `NaN` (Not a Number) or `Infinity`.
Your function should be able to correctly handle this as well.

```javascript
isNumber(42);
// => true

isNumber('Hello, World!');
// => false

isNumber(42n);
// => true

isNumber(NaN);
// => false
```

### 3. Check if a value is an object

Implement the `isObject` function, that should check if the value is an object.
On the conveyor, `null` is nothing and not considered an object.

```javascript
isObject({ greeting: 'Hello' });
// => true

isObject(25n);
// => false
```

### 4. Check if a string is numeric

Implement the `isNumericString` function, that should check if the value is a string that only consists of digits or a minus followed by digits indicating a negative number.
Only integers should be considered, decimals are not considered numeric for this check of the recycling robot.

```javascript
isNumericString(42);
// => false

isNumericString('42');
// => true

isNumericString('Hi!');
// => false
```

### 5. Check if an object is electronic

Implement the `isElectronic` function, that checks if an object is an instance of the provided `ElectronicDevice` class or one of its child classes.

```javascript
class Duck {
  //...
}

class WashingMachine extends ElectronicDevice {
  //...
}

isElectronic(new Duck());
// => false

isElectronic(new WashingMachine());
// => false
```

### 6. Check if a value is a non empty array

Implement the `isNonEmptyArray` function, that checks if a value is a non-empty array.

```javascript
isNonEmptyArray([1, 2, 3]);
// => true

isNonEmptyArray([]);
// => false
```

### 7. Check if a value is an empty array

Implement the `isEmptyArray` function, that checks if a value is an empty array.

```javascript
isEmptyArray([1, 2, 3]);
// => false

isEmptyArray([]);
// => true
```

### 8. Check if an object has a `type` property or method

Implement the `hasType` function, that checks whether an object has a `type` property or method.

```javascript
class Keyboard(){
  type(){
    // ...
  }
}
hasType({ type:"car", color:"red" })
// => true

hasType({ color:"green" })
// => false

hasType(new Keyboard())
// => true
```

### 9. Throw an error if an object does not have an `id` property or method

Implement the `assertHasId` function, that will throw an `Error` if an object is missing the `id` property.

If an object does have the `id` property, it should not return anything.

```javascript
assertHasId({ id: 42, color: 'red' });
// => undefined

assertHasId({ color: 'green' });
// Error: "Object is missing the 'id' property"
```

### 10. Check if an object has an `id` property

Implement the `hasIdProperty` function, that checks whether an object has an `id` property.

```javascript
class SimpleData {
  constructor() {
    this.number = '42';
    this.id = 'BC269327FE1D9B95';
  }
}

class StealingData extends SimpleData {}

class MethodData {
  constructor() {
    this.number = '42';
    this._id = 'BC269327FE1D9B95';
  }

  get id() {
    return this._id;
  }
}

hasIdProperty(new SimpleData());
// => true

hasIdProperty(new MethodData());
// => false

hasIdProperty(new StealingData());
// => false
```

### 11. Check if an object has a defined `type` property

Implement the `hasDefinedType` function, that checks if an object has a `type` property that is not `undefined`.

```javascript
hasDefinedType({ type: undefined, color: 'red' });
// => false

hasDefinedType({ type: 'car', color: 'green' });
// => true
```

## Source

### Created by

- @quintuple-mallard
- @SleeplessByte