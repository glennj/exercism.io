import gleam/string
import gleam/list

pub fn first_letter(name: String) {
  name |> string.trim_left |> string.slice(0, 1)
}

pub fn initial(name: String) {
  name |> first_letter |> string.uppercase |> string.append(".")
}

pub fn initials(full_name: String) {
  full_name
  |> string.split(" ")
  |> list.map(fn(name) {initial(name)})
  |> string.join(" ")
}

pub fn pair(full_name1: String, full_name2: String) {
  //      ******       ******
  //    **      **   **      **
  //  **         ** **         **
  // **            *            **
  // **                         **
  // **     X. X.  +  X. X.     **
  //  **                       **
  //    **                   **
  //      **               **
  //        **           **
  //          **       **
  //            **   **
  //              ***
  //               *
  let init1 = full_name1 |> initials
  let init2 = full_name2 |> initials
  "
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     " <> init1 <> "  +  " <> init2 <> "     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *
"
}
