(ns difference-of-squares)

(defn range1 [n]
  (range (inc n)))

(defn sum-of-squares [n]
  (reduce + (map #(* % %) (range1 n))))

(defn square-of-sum [n]
  (let [sum (reduce + (range1 n))]
    (* sum sum)))

(defn difference [n]
  (abs (- (sum-of-squares n)
          (square-of-sum n))))
