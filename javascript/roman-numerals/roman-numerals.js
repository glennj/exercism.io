/* eslint-disable  no-multi-spaces, no-loop-func */
/* eslint key-spacing: ["error", { "mode": "minimum" }] */

const ROMANATOR = [
  { cond: n => n >= 1000, numeral:  'M', value: 1000 },
  { cond: n => n >=  900, numeral: 'CM', value:  900 },
  { cond: n => n >=  500, numeral:  'D', value:  500 },
  { cond: n => n >=  400, numeral: 'CD', value:  400 },
  { cond: n => n >=  100, numeral:  'C', value:  100 },
  { cond: n => n >=   90, numeral: 'XC', value:   90 },
  { cond: n => n >=   50, numeral:  'L', value:   50 },
  { cond: n => n >=   40, numeral: 'XL', value:   40 },
  { cond: n => n >=   10, numeral:  'X', value:   10 },
  { cond: n => n >=    9, numeral: 'IX', value:    9 },
  { cond: n => n >=    5, numeral:  'V', value:    5 },
  { cond: n => n >=    4, numeral: 'IV', value:    4 },
  { cond: n => n >=    1, numeral:  'I', value:    1 },
];

export default function toRoman(decimal) {
  let roman = '';
  let n = decimal;
  while (n > 0) {
    const { numeral, value } = ROMANATOR.find(({ cond }) => cond(n));
    roman += numeral;
    n -= value;
  }
  return roman;
}

/* community
 *
 * this one's quite elegant

    module.exports = function(n) {
      var roman = '';

      function extract(c, r) {
        while (n >= c) {
          roman += r;
          n -= c;
        }
      };

      extract(1000, 'M');
      extract(900,  'CM');
      extract(500,  'D');
      extract(400,  'CD');
      extract(100,  'C');
      extract(90,   'XC');
      extract(50,   'L');
      extract(40,   'XL');
      extract(10,   'X');
      extract(9,    'IX');
      extract(5,    'V');
      extract(4,    'IV');
      extract(1,    'I');

      return roman;
    };

 *
 */
