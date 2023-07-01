(ns bank-account)

(defn open-account []
  (atom 0))

(defn close-account [acct]
  (reset! acct nil))

(defn get-balance [acct]
  (deref acct))

(defn update-balance [acct amount]
  (swap! acct + amount))
