use std::collections::HashSet;

pub fn anagrams_for<'a>(subject: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let to_key = |word: &String| -> String {
        let mut chars: Vec<char> = word.chars().collect();
        chars.sort();
        chars.iter().collect()
    };

    let lc_subject = subject.to_lowercase();
    let key = to_key(&lc_subject);

    // take 1: iterative style
    // let mut anagrams: HashSet<&'a str> = HashSet::new();
    // for candidate in possible_anagrams {
    //     let lc_candidate = candidate.to_lowercase();
    //     if lc_subject != lc_candidate && key == to_key(&lc_candidate) {
    //         anagrams.insert(candidate);
    //     }
    // }
    // anagrams

    // take 2: functional
    //
    // TODO, figure out why I need to dereference in the filter body.
    //
    possible_anagrams
        .iter()
        .map(|word| (word, word.to_lowercase()))
        .filter(|(_, lc)| *lc != lc_subject && to_key(lc) == key)
        .map(|(word, _)| word)
        .cloned()
        .collect()
}
