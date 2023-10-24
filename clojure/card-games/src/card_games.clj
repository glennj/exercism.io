(ns card-games)


(defn rounds
  "Takes the current round number and returns 
   a `list` with that round and the _next two_."
  [n]
  (list n (+ n 1) (+ n 2)))

(defn concat-rounds 
  "Takes two lists and returns a single `list` 
   consisting of all the rounds in the first `list`, 
   followed by all the rounds in the second `list`"
  [l1 l2]
  (concat l1 l2))

(defn contains-round? 
  "Takes a list of rounds played and a round number.
   Returns `true` if the round is in the list, `false` if not."
  [l n]
  (boolean (some #{n} l)))

(defn card-average
  "Returns the average value of a hand"
  [hand]
  (float (/ (reduce + hand) (count hand))))

(defn approx-average?
  "Returns `true` if average is equal to either one of:
  - Take the average of the _first_ and _last_ number in the hand.
  - Using the median (middle card) of the hand."
  [hand]
  (let [avg (card-average hand)
        approx1 (float (/ (+ (first hand) (last hand)) 2))
        approx2 (float (nth hand (/ (count hand) 2)))]
    (or (= avg approx1) (= avg approx2))))

;; ------------------------------------------------------------
;; Previous task 6
(comment
(defn average-even-odd?
  "Returns true if the average of the cards at even indexes 
   is the same as the average of the cards at odd indexes."
  [hand]
  (let [with-idx (map list hand (range (count hand)))
        evens (for [pair with-idx :when (even? (last pair))] (first pair))
        odds  (for [pair with-idx :when (odd?  (last pair))] (first pair))]
    (= (card-average evens) (card-average odds))))
)
;; ------------------------------------------------------------
(defn extract-elements-at
  "From a collection, extract elements where the index satisfies the predicate.
   This is a decorate-filter-undecorate pattern."
  [coll pred]
  (->> (map-indexed list coll)
       (filter (fn [pair] (pred (first pair))))
       (map last)))
  
(defn average-even-odd?
  "Returns true if the average of the cards at even indexes 
   is the same as the average of the cards at odd indexes."
  [hand]
  (= (card-average (extract-elements-at hand even?))
     (card-average (extract-elements-at hand odd?))))
;; ------------------------------------------------------------

(defn maybe-double-last
  "If the last card is a Jack (11), doubles its value
   before returning the hand."
  [hand]
  (let [lastcard (last hand)
        lastvalue (* lastcard (if (= 11 lastcard) 2 1))]
    (concat (butlast hand) (list lastvalue))))
