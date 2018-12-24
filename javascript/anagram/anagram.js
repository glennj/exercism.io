const sortWord = word => word.toLowerCase().split('').sort().join('');
const sameWord = (w1, w2) => w1.toLowerCase() === w2.toLowerCase();

export default class Anagram {
  constructor(str) {
    this.orig = str;
    this.key = sortWord(str);
  }

  matches(list) {
    return list.filter(word => !sameWord(this.orig, word) && this.key === sortWord(word), this);
  }
}
