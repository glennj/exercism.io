(ns matching-brackets)

(defn valid? 
  ([input]
    (valid? input '()))
  ([input stack]
    (if (empty? input)
      ;; base case, input string is empty: valid if the stack is empty
      (empty? stack)
      ;; recursive case
      (let [c (first input)]
        (case c
          ;; an open bracket: push it to the stack
          (\{ \[ \()
            (recur (rest input) (cons c stack))
          ;; a close bracket: does it match?
          (\) \] \}) 
            (case (str (first stack) c)
              ("{}" "[]" "()")
                (recur (rest input) (rest stack))
              false)
          ;; else, some other character: ignore it
          (recur (rest input) stack))))))
