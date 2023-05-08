public function anagrams(string word, string[] candidates) returns string[] {
    var lcWord = word.toLowerAscii();
    var key = keyFor(lcWord);

    return from var candidate in candidates
           let var lcCand = candidate.toLowerAscii()
           where lcWord != lcCand && key == keyFor(lcCand)
           select candidate;
}

function keyFor(string w) returns int[] => w.toCodePointInts().sort();
