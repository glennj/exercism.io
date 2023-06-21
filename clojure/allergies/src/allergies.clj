(ns allergies)

(def ALLERGENS [:eggs :peanuts :shellfish :strawberries
                :tomatoes :chocolate :pollen :cats])

(defn allergies [score]
  (->> (map-indexed list ALLERGENS)     ;;=> '((0 :eggs) (1 :peanuts) ...)
       (filter #(bit-test score (first %)))
       (map second)))

;; Can't call contains? on a LazySeq
(defn lazy-contains? [coll item]
  (boolean (some #(= item %) coll)))

(defn allergic-to? [score allergen]
  (-> (allergies score)
      (lazy-contains? allergen)))


;; Notes:
;; https://clojure-doc.org/articles/language/laziness/
