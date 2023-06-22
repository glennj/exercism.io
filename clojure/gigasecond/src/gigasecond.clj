(ns gigasecond)

(def gigasecond 1e9)

(defn from [year month day]
  ;  `bean` idea taken from 
  ;; https://exercism.org/tracks/clojure/exercises/gigasecond/solutions/JohannesFKnauf
  (-> (java.time.LocalDateTime/of year month day 0 0)
      (.plusSeconds gigasecond)
      bean
      ((juxt :year :monthValue :dayOfMonth)) ))

;  (-> (java.time.LocalDateTime/of year month day 0 0)
;      (.plusSeconds gigasecond)
;      ((juxt #(.. % getYear)
;             #(.. % getMonthValue)
;             #(.. % getDayOfMonth))) ))
