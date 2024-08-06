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

--- <!-- two-fer -->

Instead of using an `if` statement, you can provide the default value in the function signature.
<!--
See [Optional positional parameters](https://dart.dev/language/functions#optional-positional-parameters) in the manual.
-->
See [Optional positional parameters](https://dart.dev/resources/dart-cheatsheet#optional-positional-parameters) in the Dart cheatsheet.
