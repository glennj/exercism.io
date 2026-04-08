# Hints

## OO Programming

* `reset_names` is a **class method**.
  See [Class Variables][class-vars] in the MoonScript Language Guide.
* You'll need to track state across all robot instances (which names have been assigned).
  Class variables are perfect for this -- but think about what other approaches might work too.

## Performance Hints

* A good strategy for assigning names to all the robots is to generate all 676,000 names first and shuffle that list.
  Then, each new robot can simply take the next name from the shuffled list.

[class-vars]: https://moonscript.org/reference/#the-language/object-oriented-programming/class-variables