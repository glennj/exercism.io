// sadly List.isEmpty doesn't exist
let listIsEmpty = lst => lst->List.head->Option.isNone

let isPaired = input => {
  // recursively:

  let rec pairer = (chars, stack) => {
    switch chars {
    | list{} => stack->listIsEmpty
    | list{c, ...cs} =>
      switch c {
      | "{" | "[" | "(" => pairer(cs, stack->List.add(c))
      | "}" | "]" | ")" => {
        switch stack {
        | list{} => false
        | list{b, ...bs} => 
          switch b ++ c {
          | "{}" | "[]" | "()" => pairer(cs, bs)
          | _ => false
          }
        }
      }
      | _ => pairer(cs, stack)
      }
    }
  }

  pairer(input->String.split("")->List.fromArray, list{})


  /*------------------------------------------------------
   * an iterative solution:

  let str = ref(input)
  let stack = ref(list{})
  let looping = ref(true)
  let paired = ref(true)

  while looping.contents && !String.isEmpty(str.contents) {
    let c = str.contents->String.charAt(0)
    str := str.contents->String.substring(~start=1)
    switch c {
    | "{" | "[" | "(" => stack := stack.contents->List.add(c)
    | "}" | "]" | ")" => 
      switch stack.contents->List.head {
      | None => {
        paired := false
        looping := false
      }
      | Some(b) => 
        switch b ++ c {
          | "{}" | "[]" | "()" => 
              stack := stack.contents->List.tail->Option.getOr(list{})
          | _ => {
            paired := false
            looping := false
          }
        }
      }
    | _ => () 
  }
  }
  stack.contents->List.head->Option.isNone && paired.contents
  *------------------------------------------------------
  */
}
