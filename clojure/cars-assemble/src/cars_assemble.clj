(ns cars-assemble)

(defn- success-rate [speed]
  (cond (<= speed 4)  1.0
        (<= speed 8)  0.9
        (=  speed 9)  0.8
        (=  speed 10) 0.77
        :else         0.0))

(defn production-rate
  "Returns the assembly line's production rate per hour,
   taking into account its success rate"
  [speed]
  (let [cars-per-hour 221]
    (* cars-per-hour speed (success-rate speed))))

(defn working-items
  "Calculates how many working cars are produced per minute"
  [speed]
  (let [minutes-per-hour 60]
    (-> (production-rate speed)
        (/ minutes-per-hour)
        int)))
