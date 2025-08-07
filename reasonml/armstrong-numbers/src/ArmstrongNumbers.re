let intWidth = n => n -> Belt.Int.toFloat -> Js.Math.log10 -> Js.Math.ceil

let rec intPow = (num, exp) => {
  switch (exp) {
  | 0 => 1
  | _ => num * intPow(num, exp - 1)
  }
}

let armstrongSum = (num) => {
  let width = intWidth(num)
  let sum = ref(0)
  let n = ref(num)
  while (n^ > 0) {
    sum := sum^ + intPow(n^ mod 10, width)
    n := n^ / 10
  }
  // Js.Console.log((num, width, sum^))
  sum^
}

let validate = number => number == armstrongSum(number);
