# Atbash Cipher

Welcome to Atbash Cipher on Exercism's Java Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Create an implementation of the atbash cipher, an ancient encryption system created in the Middle East.

The Atbash cipher is a simple substitution cipher that relies on transposing all the letters in the alphabet such that the resulting alphabet is backwards.
The first letter is replaced with the last letter, the second with the second-last, and so on.

An Atbash cipher for the Latin alphabet would be as follows:

```text
Plain:  abcdefghijklmnopqrstuvwxyz
Cipher: zyxwvutsrqponmlkjihgfedcba
```

It is a very weak cipher because it only has one possible key, and it is a simple mono-alphabetic substitution cipher.
However, this may not have been an issue in the cipher's time.

Ciphertext is written out in groups of fixed length, the traditional group size being 5 letters, leaving numbers unchanged, and punctuation is excluded.
This is to make it harder to guess things based on word boundaries.
All text will be encoded as lowercase letters.

## Examples

- Encoding `test` gives `gvhg`
- Encoding `x123 yes` gives `c123b vh`
- Decoding `gvhg` gives `test`
- Decoding `gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt` gives `thequickbrownfoxjumpsoverthelazydog`

Since this exercise has difficulty 5 it doesn't come with any starter implementation.
This is so that you get to practice creating classes and methods which is an important part of programming in Java.
It does mean that when you first try to run the tests, they won't compile.
They will give you an error similar to:
```
 path-to-exercism-dir\exercism\java\name-of-exercise\src\test\java\ExerciseClassNameTest.java:14: error: cannot find symbol
        ExerciseClassName exerciseClassName = new ExerciseClassName();
        ^
 symbol:   class ExerciseClassName
 location: class ExerciseClassNameTest
```
This error occurs because the test refers to a class that hasn't been created yet (`ExerciseClassName`).
To resolve the error you need to add a file matching the class name in the error to the `src/main/java` directory.
For example, for the error above you would add a file called `ExerciseClassName.java`.

When you try to run the tests again you will get slightly different errors.
You might get an error similar to:
```
  constructor ExerciseClassName in class ExerciseClassName cannot be applied to given types;
        ExerciseClassName exerciseClassName = new ExerciseClassName("some argument");
                                              ^
  required: no arguments
  found: String
  reason: actual and formal argument lists differ in length
```
This error means that you need to add a [constructor](https://docs.oracle.com/javase/tutorial/java/javaOO/constructors.html) to your new class.
If you don't add a constructor, Java will add a default one for you.
This default constructor takes no arguments.
So if the tests expect your class to have a constructor which takes arguments, then you need to create this constructor yourself.
In the example above you could add:
```
ExerciseClassName(String input) {

}
``` 
That should make the error go away, though you might need to add some more code to your constructor to make the test pass!

You might also get an error similar to:
```
  error: cannot find symbol
        assertThat(exerciseClassName.someMethod()).isEqualTo(expectedOutput);
                                     ^
  symbol:   method someMethod()
  location: variable exerciseClassName of type ExerciseClassName
```
This error means that you need to add a method called `someMethod` to your new class.
In the example above you would add:
```
String someMethod() {
  return "";
}
```
Make sure the return type matches what the test is expecting.
You can find out which return type it should have by looking at the type of object it's being compared to in the tests.
Or you could set your method to return some random type (e.g. `void`), and run the tests again.
The new error should tell you which type it's expecting.

After having resolved these errors you should be ready to start making the tests pass!

## Source

### Contributed to by

- @c-thornton
- @FridaTveit
- @jackattack24
- @jmluy
- @jmrunkle
- @jtigger
- @kytrinyx
- @lemoncurry
- @LuLechuan
- @matthewmorgan
- @mirkoperillo
- @msomji
- @muzimuzhi
- @PaulNoth
- @rohit1104
- @sjwarner-bp
- @SleeplessByte
- @Smarticles101
- @sshine
- @stkent
- @vdemeester
- @Zaldrick
- @Zhiyuan-Amos

### Based on

Wikipedia - https://en.wikipedia.org/wiki/Atbash