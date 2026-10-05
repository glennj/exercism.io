// ReScript ints are 32-bit integers, too small for the biggest resistor (99 billion)

let label = colors => {
  assertEqual(Array.length(colors) >= 3, true)

  let val = ref(
    BigInt.mul(
      BigInt.fromInt(ResistorColorDuo.value(Array.slice(colors, ~end=2))),
      BigInt.fromInt(10 ** ResistorColor.colorCode(Array.getUnsafe(colors, 2)))
    )
  )
  let i = ref(0)

  // BigInt literals use "n" suffix
  if val.contents > 0n {
    while val.contents->BigInt.mod(1000n) == 0n {
      val := val.contents->BigInt.div(1000n)
      i := i.contents + 1
    }
  }
  let prefix = ["", "kilo", "mega", "giga"]->Array.getUnsafe(i.contents)

  `${val.contents->BigInt.toString} ${prefix}ohms`
}
