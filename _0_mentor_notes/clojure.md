# Clojure student notes

## reverse-string

I used a recursive solution.
- discovered that the `recur` function is required for tailcall optimization.

### community solutions for later study

- https://exercism.org/tracks/clojure/exercises/reverse-string/solutions/llk23r
  ```clojure
  (defn reverse-string [s]
    (apply str (into () s)))
  ```

- https://exercism.org/tracks/clojure/exercises/reverse-string/solutions/JohnnyLT
  ```clojure
  (defn reverse-string [s]
    (reduce #(str %2 %1) "" s))
  ```
- https://exercism.org/tracks/clojure/exercises/reverse-string/solutions/fernandezpablo85

  multi-arity and destructuring and recursive
  ```clojure
  (defn reverse-string
    ([string]
      (reverse-string string ""))
    ([[head & tail] acc]
      (if (nil? head) acc
          (recur tail (str head acc)))))
  ```
