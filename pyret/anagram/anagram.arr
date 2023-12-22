use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: find-anagrams end

fun find-anagrams(phrase, candidates):
  key = lam(s): string-explode(s).sort() end

  subject = string-to-lower(phrase)
  subj-key = key(subject)

  candidates.filter(lam(c):
    lc = string-to-lower(c)
    not(lc == subject) and (key(lc) == subj-key)
  end)
end
