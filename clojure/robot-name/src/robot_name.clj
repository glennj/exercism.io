(ns robot-name)

;; global state: all-names is a atom of a set
(def all-names (atom #{}))

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
;; Not efficient for generating hundreds of thousands of names:
;; the better strategy is to generate all possible names and shuffle them.
(defn reset-name [robot]
  (let [new-name (rand-name)]
    (if (contains? @all-names new-name)
      (recur robot)
      (do
        (reset! all-names (conj @all-names new-name))
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
