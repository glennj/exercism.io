use std::vec;

// Hmm, we iterate over the input essentially 3 times here...
//
// I'm going to say that it's cheating to use rev()
//
pub fn reverse(input: &str) -> String {
    let size = input.chars().count();

    let mut chars = vec![' '; size];
    for (i, c) in input.chars().enumerate() {
        chars[size - i - 1] = c;
    }

    // let mut reversed = String::new();
    // for c in chars {
    //     reversed.push(c);
    // }
    // reversed

    chars.iter().collect()
}
