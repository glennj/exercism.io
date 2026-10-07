let find = (haystack, needle) => {
  let rec finder = (left, right) => {
    let mid = (left + right) / 2
    let elem = Array.getUnsafe(haystack, mid)
    switch true {
    | _ if left > right    => None
    | _ if needle == elem  => Some(mid)
    | _ if needle < elem   => finder(left, mid - 1)
    | _                    => finder(mid + 1, right)
    }
  }
  
  finder(0, Array.length(haystack) - 1)
}