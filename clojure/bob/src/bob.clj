(ns bob)

(defn re-match? [re s]
  (-> (re-find re s)
      nil?
      not))

(defn yelling? [s]
  ;; contains an upper and does not contain a lower
  (and (re-match? #"\p{Lu}" s)
       (not (re-match? #"\p{Ll}" s))))

(defn question? [s]
  (re-match? #"\?\s*$" s))

(defn silent? [s]
  ;; does not contain a non-space
  (not (re-match? #"\S" s)))

(defn response-for [s]
  (cond
    (silent? s)                      "Fine. Be that way!"
    (and (yelling? s) (question? s)) "Calm down, I know what I'm doing!"
    (yelling? s)                     "Whoa, chill out!"
    (question? s)                    "Sure."
    :else                            "Whatever."))
