let isPaired = input => {
  // recursively:

  let rec pairer = (chars, stack) => {
    switch chars {
    | list{} => stack->List.head->Option.isNone // sadly List.isEmpty doesn't exist
    | list{c, ...rest} =>
      switch c {
      | "{" | "[" | "(" => pairer(rest, stack->List.add(c))
      | "}" | "]" | ")" => {
        switch stack {
        | list{} => false
        | list{b, ...brackets} => 
          switch b + c {
          | "{}" | "[]" | "()" => pairer(rest, brackets)
          | _ => false
          }
        }
      }
      | _ => pairer(rest, stack)
      }
    }
  }

  pairer(input->String.split("")->List.fromArray, list{})


  /* an iterative solution:

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
        switch b + c {
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
  */
}
