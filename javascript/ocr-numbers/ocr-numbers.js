/* eslint-disable  class-methods-use-this */

const strings = [
  ' _ | ||_|   ',
  '     |  |   ',
  ' _  _||_    ',
  ' _  _| _|   ',
  '   |_|  |   ',
  ' _ |_  _|   ',
  ' _ |_ |_|   ',
  ' _   |  |   ',
  ' _ |_||_|   ',
  ' _ |_| _|   ',
];

class Ocr {
  convertLine(lines) {
    let word = '';
    for (let i = 0; i < lines[0].length; i += 3) {
      const str = lines.map(l => l.slice(i, i + 3)).join('');
      const idx = strings.indexOf(str);
      word = word.concat(idx === -1 ? '?' : idx);
    }
    return word;
  }

  convert(text) {
    const lines = text.split('\n');
    if (lines.length % 4 !== 0) throw new Error('a');
    if (!lines.every(l => l.length % 3 === 0)) throw new Error('c');

    const words = [];
    for (let i = 0; i < lines.length; i += 4) {
      words.push(this.convertLine(lines.slice(i, i + 4)));
    }
    return words.join(',');
  }
}

module.exports = Ocr;
