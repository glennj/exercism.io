(ns elyses-destructured-enchantments)

(defn first-card
  "Returns the first card from deck."
  [[a & deck]]
  a)

(defn second-card
  "Returns the second card from deck."
  [[_ b & deck]]
  b)

(defn swap-top-two-cards
  "Returns the deck with first two items reversed."
  [[a b & deck]]
  (concat [b a] deck))

(defn discard-top-card
  "Returns a sequence containing the first card and
   a sequence of the remaining cards in the deck."
  [[a & deck]]
    [a deck])

(def face-cards
  ["jack" "queen" "king"])

(defn insert-face-cards
  "Returns the deck with face cards between its head and tail."
  [[a & deck]]
    (if a
      (apply concat [[a] face-cards deck])
      face-cards))