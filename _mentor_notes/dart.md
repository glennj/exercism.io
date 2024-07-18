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
