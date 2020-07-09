#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

// take 2: pattern matching
pub fn sublist<T: PartialEq>(first: &[T], second: &[T]) -> Comparison {
    use Comparison::*;

    match (first.len(), second.len()) {
        (0, 0) => return Equal,
        (0, _) => return Sublist,
        (_, 0) => return Superlist,
        (i, j) if i > j => {
            if first.windows(j).any(|list| list == second) {
                return Superlist;
            }
        }
        (i, j) if i < j => {
            if second.windows(i).any(|list| list == first) {
                return Sublist;
            }
        }
        (_, _) => {
            if first == second {
                return Equal;
            }
        }
    };
    return Unequal;
}

////////////////////////////////////////////////////////////
// First take, iteratate over lists
//
//  fn compare_lists<T: PartialEq>(
//      shorter: &[T],
//      longer: &[T],
//      short_len: usize,
//      long_len: usize,
//      true_result: Comparison,
//  ) -> Comparison {
//      if short_len == 0 {
//          return true_result;
//      }
//
//      let mut i = 0;
//      while i <= long_len - short_len {
//          if longer[i] == shorter[0] {
//              let mut j = 1;
//              let mut matched = true;
//              while j < short_len {
//                  if longer[i + j] != shorter[j] {
//                      matched = false;
//                      break;
//                  }
//                  j += 1;
//              }
//              if matched {
//                  return true_result;
//              }
//          }
//          i += 1;
//      }
//      return Comparison::Unequal;
//  }
//
//  pub fn sublist<T: PartialEq>(first_list: &[T], second_list: &[T]) -> Comparison {
//      let first_len = first_list.len();
//      let second_len = second_list.len();
//
//      if first_len == second_len {
//          compare_lists(
//              &first_list,
//              &second_list,
//              first_len,
//              second_len,
//              Comparison::Equal,
//          )
//      } else if first_len < second_len {
//          compare_lists(
//              &first_list,
//              &second_list,
//              first_len,
//              second_len,
//              Comparison::Sublist,
//          )
//      } else {
//          compare_lists(
//              &second_list,
//              &first_list,
//              second_len,
//              first_len,
//              Comparison::Superlist,
//          )
//      }
//  }
