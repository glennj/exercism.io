(ns say)

(def ones
  ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven"
   "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])

(def tens
  [nil nil "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])

(def bases
  {"hundred"  100
   "thousand" (int 1e3)
   "million"  (int 1e6)
   "billion"  (int 1e9)})

(declare small-number big-number)

(defn number [num]
  (cond
    (< num 0)    (throw (IllegalArgumentException. "num cannot be negative"))
    (< num 20)   (get ones num)
    (< num 100)  (small-number num)
    (< num 1e3)  (big-number   num "hundred")
    (< num 1e6)  (big-number   num "thousand")
    (< num 1e9)  (big-number   num "million")
    (< num 1e12) (big-number   num "billion")
    :else        (throw (IllegalArgumentException. "num too large"))))

(defn- divmod [num base]
  [(quot num base) (rem num base)])

(defn- small-number [num]
  (let [[q r] (divmod num 10)]
    (str (get tens q)
         (when-not (zero? r) (str "-" (get ones r))))))

(defn- big-number [num unit]
  (let [base (get bases unit)
        [q r] (divmod num base)]
    (str (number q) " " unit
         (when-not (zero? r) (str " " (number r))))))
