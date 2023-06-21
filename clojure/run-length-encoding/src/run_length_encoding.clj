(ns run-length-encoding
  (:require [clojure.string :as str]))

(declare str-repeat to-int)

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
    (str/replace plain-text
                 #"(.)\1+"
                 #(str (count (get % 0))
                       (get % 1))))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
    (str/replace cipher-text
                 #"(\d+)(.)"
                 #(str-repeat (to-int (get % 1))
                              (get % 2))))


(defn str-repeat [n s]
  (-> (repeat n s)
      (str/join)))

(defn to-int [s]
  (Integer/parseInt s 10))
