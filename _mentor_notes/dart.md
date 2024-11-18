for contributing, the online format checker isn't exactly like `dart format`.
Use
```
cd repo_root
rm -r .dart_tool pubspec.lock
dart run dart_style:format -i 0 -l 120 -w exercises/practice/circular-buffer/{.meta/lib,lib,test}/
git checkout pubspec.lock
```

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
The return value from the 2nd argument becomes the accumulator value for the next iteration.

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

<!-- main -->

The presence of a `main` function indicates you're not using the provided tests.
Please see [Testing on the Dart track](https://exercism.org/docs/tracks/dart/tests).
