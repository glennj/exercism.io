// Map a roman numeral to the decimal value.
// Maps remember insertion order.
const R2D = new Map<string,number>()

R2D.set('M', 1000); R2D.set('CM', 900); R2D.set('D', 500); R2D.set('CD', 400)
R2D.set('C',  100); R2D.set('XC',  90); R2D.set('L',  50); R2D.set('XL',  40)
R2D.set('X',   10); R2D.set('IX',   9); R2D.set('V',   5); R2D.set('IV',   4)
R2D.set('I',    1)

export function toRoman(decimal: number): string {
  let roman = ''
  for (const [r, d] of R2D) {
    while (decimal >= d) {
      roman   += r
      decimal -= d
    }
  }
  return roman
}
