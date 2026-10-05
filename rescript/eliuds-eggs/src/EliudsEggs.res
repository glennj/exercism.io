let eggCount = num => {
  
  /* Iterative:
  let n = ref(num)
  let count = ref(0)
  while n.contents > 0 {
    if n.contents->Int.bitwiseAnd(1) == 1 {
      count := count.contents + 1
    }
    n := n.contents->Int.shiftRight(1)
  }
  count.contents
  */

  // Recursive:
  let rec counter = (n, count) =>
    if n == 0 {
      count
    } else {
      counter(Int.shiftRight(n, 1), count + Int.bitwiseAnd(n, 1))
    }
  counter(num, 0)
}
