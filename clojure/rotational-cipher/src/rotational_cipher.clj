(ns rotational-cipher
  (:require [clojure.string :as str]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn rotate-alphabet [rotation]
  (let [n (mod rotation (count alphabet)) 
        rotated (concat (drop n alphabet) (take n alphabet))]
    (zipmap (concat alphabet (str/upper-case alphabet))
            (concat rotated (map str/upper-case rotated)))))

(defn rotate [phrase rotation]
  (let [rotn (rotate-alphabet rotation)]
    (->> phrase
         (map #(get rotn % %))
         (str/join ""))))
