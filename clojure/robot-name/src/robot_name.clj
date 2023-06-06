(ns robot-name)

;; global state: all-names is a reference to a set
(def all-names (ref #{}))

;; forward declarations
(declare robot-name reset-name rand-name)

;; A robot's name is required to be mutable.
;; Let a robot be an atom of a string.
(defn robot []
  (let [robot (atom "")]
    (reset-name robot)
    robot))

(defn robot-name [robot]
  (deref robot))

;; Mutates the robot, and returns the new name.
;; Not efficient for generating hundreds of thousands of names.
(defn reset-name [robot]
  (let [new-name (rand-name)]
    (if (contains? @all-names new-name)
      (recur robot)
      (dosync
        (alter all-names conj new-name)
        (reset! robot new-name)))))

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn rand-name []
  (str (rand-nth alphabet)
       (rand-nth alphabet)
       (rand-int 10)
       (rand-int 10)
       (rand-int 10)))

;; https://clojure.org/reference/atoms
;; https://clojure.org/reference/refs
