def find_anagrams(word, candidates):
    anagrams = []
    wl = word.lower()
    key = sorted(wl)
    for candidate in candidates:
        cl = candidate.lower()
        if cl != wl and sorted(cl) == key:
            anagrams.append(candidate)
    return anagrams
