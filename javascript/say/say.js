/* eslint-disable  no-multi-spaces, class-methods-use-this, no-param-reassign */

const numbers = [];
numbers[0] = 'zero';  numbers[10] = 'ten';
numbers[1] = 'one';   numbers[11] = 'eleven';
numbers[2] = 'two';   numbers[12] = 'twelve';    numbers[20] = 'twenty';
numbers[3] = 'three'; numbers[13] = 'thirteen';  numbers[30] = 'thirty';
numbers[4] = 'four';  numbers[14] = 'fourteen';  numbers[40] = 'forty';
numbers[5] = 'five';  numbers[15] = 'fifteen';   numbers[50] = 'fifty';
numbers[6] = 'six';   numbers[16] = 'sixteen';   numbers[60] = 'sixty';
numbers[7] = 'seven'; numbers[17] = 'seventeen'; numbers[70] = 'seventy';
numbers[8] = 'eight'; numbers[18] = 'eightteen'; numbers[80] = 'eighty';
numbers[9] = 'nine';  numbers[19] = 'nineteen';  numbers[90] = 'ninety';

const group = ['', ' thousand', ' million', ' billion', ' trillion'];

const divmod = (num, div) => [Math.floor(num / div), num % div];

export class Say {
  inEnglish(n) {
    if (!Number.isInteger(n)) throw new Error();
    if (n < 0 || n > 999999999999) {
      throw new Error('Number must be between 0 and 999,999,999,999.');
    }
    if (n === 0) return numbers[0];
    const words = [];
    let g = 0;
    do {
      let groupAdded = false;
      let hundreds;
      [n, hundreds] = divmod(n, 1000);
      if (hundreds > 0) {
        const [hun, tens] = divmod(hundreds, 100);
        if (tens > 0) {
          if (numbers[tens] !== undefined) {
            words.unshift(`${numbers[tens]}${group[g]}`);
          } else {
            const [ten, ones] = divmod(tens, 10);
            words.unshift(`${numbers[10 * ten]}-${numbers[ones]}${group[g]}`);
          }
          groupAdded = true;
        }
        if (hun > 0) words.unshift(`${numbers[hun]} hundred${groupAdded ? '' : group[g]}`);
      }
      g += 1;
    } while (n > 0);
    return words.join(' ');
  }
}
