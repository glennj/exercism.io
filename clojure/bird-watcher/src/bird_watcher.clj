(ns bird-watcher)

(def last-week [0 2 5 3 7 8 4])

(defn today [birds]
  (last birds))

(defn inc-bird [birds]
  (let [last-index (dec (count birds))
        bird-count (today birds)]
    (assoc birds last-index (inc bird-count))))

(defn day-without-birds? [birds]
  ; grrr: `some` returns nil if none found, but the test checks the result is _equal_ to false.
  (->> birds (some zero?) boolean))

(defn n-days-count [birds n]
  (reduce + 0 (subvec birds 0 n)))

(defn busy-days [birds]
  (let [busy-count 5]
    (->> birds
         (filter #(>= % busy-count))
         count))
  )

(defn odd-week? [birds]
  (let [odd-pattern [1 0 1 0 1 0 1]]
    (= birds odd-pattern)))
