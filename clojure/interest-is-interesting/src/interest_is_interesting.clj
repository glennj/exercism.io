(ns interest-is-interesting)

(defn interest-rate
  "Return the interest rate, given the balance, as a double."
  [balance]
  (cond (< balance 0)   -3.213
        (< balance 1000) 0.5
        (< balance 5000) 1.621
        :else            2.475))

(defn annual-balance-update
  "Return the updated balace, adding the interest earned, as a decimal."
  [balance]
  (let [interest        (bigdec (interest-rate balance))
        abs             (if (neg? balance) -1M 1M)
        interest-earned (* abs interest 0.01M balance)]
    (+ balance interest-earned)))

(defn amount-to-donate
  "Calculate the donation amount as an integer."
  [balance tax-free-percentage]
  (let [generosity-factor 2.0]
    (-> (* tax-free-percentage 0.01)
        (* generosity-factor)
        (* balance)
        (max 0.0)
        int)))
