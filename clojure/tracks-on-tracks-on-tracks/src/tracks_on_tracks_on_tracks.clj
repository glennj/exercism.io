(ns tracks-on-tracks-on-tracks)

(defn new-list
  "Creates an empty list of languages to practice."
  []
  (list))  ;; note: simple () and quoted '() work too.

(defn add-language
  "Adds a language to the list."
  [lang-list lang]
  (cons lang lang-list))  ;; TODO cons hint should be in introduction

(defn first-language
  "Returns the first language on the list."
  [lang-list]
  (first lang-list))  ;; TODO first hint should be in introduction

(defn remove-language
  "Removes the first language added to the list."
  [lang-list]
  (rest lang-list))  ;; TODO rest hint should be in introduction

(defn count-languages
  "Returns the total number of languages on the list."
  [lang-list]
  (count lang-list))

(defn learning-list
  "Creates an empty list, adds Clojure and Lisp, removes Lisp, adds
  Java and JavaScript, then finally returns a count of the total number
  of languages."
  []
  ;; naive approach
  ;(def ll (new-list))
  ;(def ll (add-language ll "Clojure"))
  ;(def ll (add-language ll "Lisp"))
  ;(def ll (remove-language ll))
  ;(def ll (add-language ll "Java"))
  ;(def ll (add-language ll "JavaScript"))
  ;(count-languages ll)
  (-> (new-list)
      (add-language "Clojure")
      (add-language "Lisp")
      (remove-language)
      (add-language "Java")
      (add-language "JavaScript")
      (count-languages)))
