(ns reverse-string)

; stack overflow will happen, unless we use `recur`
; to specify that yes, we have a tailcall.
(defn reverse-string [s]
  (letfn [(rev [s r]
            (if (empty? s)
              r
              (recur (subs s 1) (str (first s) r))))]
    (rev s "")))
