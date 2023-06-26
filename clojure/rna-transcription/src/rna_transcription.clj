(ns rna-transcription)

(defn to-rna [dna]
  (let [rna-nucleotides (map '{\G \C, \C \G, \T \A, \A \U} dna)]
    (if (not-any? nil? rna-nucleotides)
      (clojure.string/join "" rna-nucleotides)
      (throw (AssertionError. "invalid DNA nucleotide")))))
