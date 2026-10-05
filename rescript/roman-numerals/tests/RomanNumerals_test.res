open Test
open RomanNumerals

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("1 is I", () => {equal(~message="1 is I", roman(1), "I")})

test("2 is II", () => {equal(~message="2 is II", roman(2), "II")})

test("3 is III", () => {equal(~message="3 is III", roman(3), "III")})

test("4 is IV", () => {equal(~message="4 is IV", roman(4), "IV")})

test("5 is V", () => {equal(~message="5 is V", roman(5), "V")})

test("6 is VI", () => {equal(~message="6 is VI", roman(6), "VI")})

test("9 is IX", () => {equal(~message="9 is IX", roman(9), "IX")})

test("16 is XVI", () => {equal(~message="16 is XVI", roman(16), "XVI")})

test("27 is XXVII", () => {equal(~message="27 is XXVII", roman(27), "XXVII")})

test("48 is XLVIII", () => {equal(~message="48 is XLVIII", roman(48), "XLVIII")})

test("49 is XLIX", () => {equal(~message="49 is XLIX", roman(49), "XLIX")})

test("59 is LIX", () => {equal(~message="59 is LIX", roman(59), "LIX")})

test("66 is LXVI", () => {equal(~message="66 is LXVI", roman(66), "LXVI")})

test("93 is XCIII", () => {equal(~message="93 is XCIII", roman(93), "XCIII")})

test("141 is CXLI", () => {equal(~message="141 is CXLI", roman(141), "CXLI")})

test("163 is CLXIII", () => {equal(~message="163 is CLXIII", roman(163), "CLXIII")})

test("166 is CLXVI", () => {equal(~message="166 is CLXVI", roman(166), "CLXVI")})

test("402 is CDII", () => {equal(~message="402 is CDII", roman(402), "CDII")})

test("575 is DLXXV", () => {equal(~message="575 is DLXXV", roman(575), "DLXXV")})

test("666 is DCLXVI", () => {equal(~message="666 is DCLXVI", roman(666), "DCLXVI")})

test("911 is CMXI", () => {equal(~message="911 is CMXI", roman(911), "CMXI")})

test("1024 is MXXIV", () => {equal(~message="1024 is MXXIV", roman(1024), "MXXIV")})

test("1666 is MDCLXVI", () => {equal(~message="1666 is MDCLXVI", roman(1666), "MDCLXVI")})

test("3000 is MMM", () => {equal(~message="3000 is MMM", roman(3000), "MMM")})

test("3001 is MMMI", () => {equal(~message="3001 is MMMI", roman(3001), "MMMI")})

test("3888 is MMMDCCCLXXXVIII", () => {
  equal(~message="3888 is MMMDCCCLXXXVIII", roman(3888), "MMMDCCCLXXXVIII")
})

test("3999 is MMMCMXCIX", () => {equal(~message="3999 is MMMCMXCIX", roman(3999), "MMMCMXCIX")})
