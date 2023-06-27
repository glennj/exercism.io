(ns nucleotide-count)

(def init-nucleotide-count {\A 0 \C 0 \G 0 \T 0})

(defn validate-nucleotide [nucleotide]
  (when-not (find init-nucleotide-count nucleotide)
    (throw (IllegalArgumentException. "invalid nucleotide"))))

(defn validate-strand [strand]
  (loop [[h & t] strand]
    (when h
      (validate-nucleotide h)
      (recur t))))

(defn nucleotide-counts [strand]
  (validate-strand strand)
  (->> (frequencies strand)
       (merge init-nucleotide-count)))

(defn count-of-nucleotide-in-strand [nucleotide strand]
  (validate-nucleotide nucleotide)
  (-> (nucleotide-counts strand)
      (get nucleotide)))
