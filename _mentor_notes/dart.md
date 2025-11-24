<!-- Contributing -->
for contributing, the online format checker isn't exactly like `dart format`.
Use
```
cd repo_root
rm -r .dart_tool pubspec.lock
dart run dart_style:format -i 0 -l 120 -w exercises/practice/circular-buffer/{.meta/lib,lib,test}/
git checkout pubspec.lock
```

<!-- Docs and tips -->

[Effective Dart](https://dart.dev/effective-dart) is a very useful guide to community best practices.

---

Since you have a function that returns a single expression, you can write it with [arrow notation](https://dart.dev/language/functions)
```dart
int add(int a, int b) {
  return a + b;
}
// or
int add(int a, int b) => a + b;
```

This isn't necessarily an "improvement", more a matter of style.

---

This is a common anti-pattern:
```dart
if (cond) {
  return true;
} else {
  return false;
}
```
`cond` is already an expression that evaluates to a boolean value. You can return it directly.
```dart
return cond;
```

And then, if that's the only code in the function
```dart
bool myFunc(...) {
  return cond;
}
```
<!--
then you can use Dart's [arrow notation](https://dart.dev/language/functions)
-->
then you can use Dart's [arrow notation](https://dart.dev/resources/dart-cheatsheet#arrow-syntax)
```dart
bool myFunc(...) => cond;
```

---

When you find yourself doing
```dart
var result = someValue;
for (var elem in someList) {
    result = result + someFunc(elem)
}
```

you can use a more functional style with [`fold`](https://api.dart.dev/stable/3.5.0/dart-core/Iterable/fold.html)
```dart
var result = someList.fold(
    someValue,
    (acc, elem) => acc + someFunc(elem)
);
```
The return value from fold's 2nd argument becomes the accumulator value for the next iteration.

---

Function variables usually don't need to have an explicit type. 
Instead of:
```dart
String wordUpper = word.toUpperCase();
```
you can use 
```dart
var wordUpper = word.toUpperCase();
```

See the [manual](https://dart.dev/language#variables) and the ["Effective Dart" doc](https://dart.dev/effective-dart/usage#do-follow-a-consistent-rule-for-var-and-final-on-local-variables).

---

Regular Expressions:

For further reading:
* [the Dart RegExp API docs](https://api.dart.dev/stable/3.5.3/dart-core/RegExp-class.html)
* [the ECMAScript regex specification](https://262.ecma-international.org/9.0/#sec-regexp-regular-expression-objects) -- very technical
* [JavaScript regex docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Regular_expressions)

---

<!-- two-fer -->

Instead of using an `if` statement, you can provide the default value in the function signature.
See [Optional positional parameters](https://dart.dev/language/functions#optional-positional-parameters) in the manual.
<!--
See [Optional positional parameters](https://dart.dev/resources/dart-cheatsheet#optional-positional-parameters) in the Dart cheatsheet.
-->

---

Well done! Many people miss the [optional positional parameters](https://dart.dev/language/functions#optional-positional-parameters).

---

<!-- leap -->

Can you combine the tests into one boolean expression (using [Logical operators](https://dart.dev/language/operators#logical-operators))?
It often helps to say the leap year rules out loud using the words "and" and "or" and "not".

---

<!-- eliuds-eggs -->

This is a good exercise to practice with [bitwise operators](https://dart.dev/language/operators#bitwise-and-shift-operators).

---

<!-- scrabble -->

The scores map can be placed outside the function as a library constant:

```dart
const scoring = { ... };

int score(String s) { ... }
```

---

<!-- atbash -->

Notice that (aside from adding spaces) the encoding algorithm is exactly the same as the decoding algorithm: map 'a' to 'z', 'b' to 'y', etc.
There's an opportunity to refactor to remove the duplicated code.


---

<!-- main -->

The presence of a `main` function indicates you're not using the provided tests.
Please see [Testing on the Dart track](https://exercism.org/docs/tracks/dart/tests).

---

<!-- string indexing -->

<details><summary>
You have to be careful using string indexing, particularly for Unicode strings.
While there are no Unicode strings in this exercise, it's helpful to know.
Click for details.
</summary>

---

In Dart, string indexing uses neither byte nor what we perceive as character indexing directly. Instead, it operates on UTF-16 code units.
Here's what that means:

* A Dart [String](https://api.dart.dev/dart-core/String-class.html) is a sequence of UTF-16 code units.
* [`string[i]`](https://api.dart.dev/dart-core/String/operator_get.html) accesses the i-th 16-bit code unit, not the i-th character.
* Most common characters (like those in ASCII) are represented by a single UTF-16 code unit, so it often feels like character indexing.
* However, some characters, like many emojis or complex symbols, require two UTF-16 code units (a "surrogate pair").
  In these cases, indexing will access only half of the character, which can lead to errors.


```dart
var myString = 'dart';
print(myString[0]); // Output: 'd'

var emojiString = '🎯';    // A single character, but a surrogate pair in UTF-16
print(emojiString.length); // Output: 2
// print(emojiString[0]);  // This would be an invalid character on its own.
```

To correctly access characters, you should use the [`runes`](https://api.dart.dev/dart-core/String/runes.html) property, which iterates over Unicode code points:

```dart
for (var rune in emojiString.runes) {
  print(String.fromCharCode(rune)); // Output: 🎯
}
```
</details>

<!--
For handling user-perceived characters (grapheme clusters), such as an emoji with a skin-tone modifier, it's recommended to use the `characters` (https://pub.dev/packages/characters) package.
-->
