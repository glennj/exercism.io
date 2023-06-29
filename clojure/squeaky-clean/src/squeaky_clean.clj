(ns squeaky-clean
  (:require [clojure.string :as str]))

;; ... now I have two problems ...
(defn clean [s]
  (-> s
      (str/replace #"\s" "_")                                 ; replace whitespace
      (str/replace #"\p{IsControl}" "CTRL")                   ; replace control chars
      (str/replace #"-(\p{Ll})" #(str/upper-case (get % 1)))  ; convert kebab to camel
      (str/replace #"[^_\p{L}]" "")                           ; remove non letters
      (str/replace #"[\p{IsGreek}&&\p{IsLowercase}]" "")))    ; remove greek lower


;; ----------------------------------------------------------
;; using non-regexp functions
;;
;(defn- replace-spaces [s]
;  (->> s (map #(if (Character/isWhitespace %) \_ %)) str/join))
;
;(defn- replace-control-chars [s]
;  (->> s (map #(if (Character/isISOControl %) "CTRL" %)) str/join))
;
;(defn- ucfirst [word]
;  (str/join (cons (str/upper-case (first word))
;                  (rest word))))
;
;(defn convert-kebab-to-camel [s]
;  (let [words (str/split s #"-")]
;    (loop [result [(first words)], words (rest words)]
;      (if (empty? words)
;        (str/join result)
;        (recur (conj result (ucfirst (first words)))
;               (rest words))))))
;
;(defn clean [s]
;  (-> s 
;      replace-spaces
;      replace-control-chars
;      convert-kebab-to-camel
;      remove-non-letters
;      remove-greek-lower))
