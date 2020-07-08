/// Check a Luhn checksum.

////////////////////////////////////////////////////////////
// Second take, a closure
pub fn is_valid(code: &str) -> bool {
    let cleaned = code.replace(" ", "");
    if cleaned.len() <= 1 || cleaned.contains(|c: char| !c.is_digit(10)) {
        return false;
    }

    let double = |n: u32| {
        match n {
            0..=4 => n * 2,
            _ => n * 2 - 9,
        }
    };
    let mut sum = 0;

    for (idx, ch) in cleaned.chars().rev().enumerate() {
        let digit = ch.to_digit(10).unwrap();
        if idx % 2 == 0 {
            sum += digit
        } else {
            sum += double(digit);
        }
    }
    return sum % 10 == 0;
}

////////////////////////////////////////////////////////////
// First take: use an array to store the doubled values.
// Indexing into an array can be annoying...
//
// use std::convert::TryInto;
// pub fn is_valid(code: &str) -> bool {
//     let cleaned = code.replace(" ", "");
//     if cleaned.len() <= 1 || cleaned.contains(|c: char| !c.is_digit(10)) {
//         return false;
//     }
// 
//     let doubles = [0, 2, 4, 6, 8, 1, 3, 5, 7, 9];
//     let mut sum = 0;
// 
//     for (idx, ch) in cleaned.chars().rev().enumerate() {
//         let digit = ch.to_digit(10).unwrap();
//         if idx % 2 == 0 {
//             sum += digit
//         } else {
//             // arg, can't index into an array with a u32
//             let i: usize = digit.try_into().unwrap();
//             sum += doubles[i];
//         }
//     }
//     return sum % 10 == 0;
// }
