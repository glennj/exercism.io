# Exercism Bootcamp Notes

## Loops

Comparing Jiki loop mechanisms to a Javascript-like language

### loop over the items of a list

* Javascript-ish

  ```javascript
  for (item of list) {
    ...
  }
  ```

* Jiki

  ```javascript
  for each item in list do
    ...
  end
  ```

### loop over the items of a list, with index

* Javascript-ish

  ```javascript
  list.forEach((item, idx) => ...);
  ```

* Jiki

  ```javascript
  for each item in list indexed by i do
    ...
  end
  ```

### loop over a key-value dictionary

* Javascript-ish

  ```javascript
  for (key in dict) {
    value = dict[key];
    ...
  }
  ```
  or
  ```javascript
  aMap.forEach((value, key) => ...);
  // ...........^^^^^^^^^^
  ```

* Jiki

  ```javascript
  for each key, value in dict do
    // ....^^^^^^^^^^
    ...
  end
  ```


### loop forever

* Javascript-ish

  ```javascript
  while (1) {
    ...
  }
  ```

* Jiki

  ```javascript
  repeat_forever do
    ...
  end
  ```

### loop forever, with counting

* Javascript-ish

  ```javascript
  i = 1
  while (1) {
    ...
    i++;
  }
  ```
  or
  ```javascript
  for (i = 1; 1; i++) {
    ...
  }
  ```

* Jiki

  ```javascript
  repeat_forever indexed by i do
    ...
  end
  ```

### C-like for loop, starting at 1, incrementing by 1

* Javascript-ish

  ```javascript
  for (i = 1; i <= n; i++) {
    ...
  }
  ```

* Jiki

  ```javascript
  repeat n times indexed by i do
    ...
  end
  ```

### C-like for loop, _not_ starting at 1, incrementing by any step

* Javascript-ish

  ```javascript
  for (i = initial; i <= n; i += step) {
    ...
  }
  ```

* Jiki

  ```javascript
  set i to initial
  repeat_forever do
    if i > n do
      break
    end
    ...
    change i to i + step
  end
  ```

### while loop

* Javascript-ish

  ```javascript
  while (some_boolean_function()) {
    ...
  }
  ```

* Jiki

  ```javascript
  repeat_forever do
    if not some_boolean_function() do
      break
    end
    ...
  end
  ```
