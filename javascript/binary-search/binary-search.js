/* eslint no-underscore-dangle: ["error", { "allowAfterThis": true }] */

const bsearch = (list, element, i = 0, j = list.length - 1) => {
  if (i >= j) return list[i] === element ? i : -1;
  const mid = i + Math.ceil((j - i) / 2);
  if (list[mid] === element) return mid;
  const nextRange = element < list[mid] ? [i, mid - 1] : [mid + 1, j];
  return bsearch(list, element, ...nextRange);
};

class BinarySearch {
  constructor(list) {
    this.array = list;
  }

  get array() { return this._array; }

  set array(list) {
    for (let i = 1; i < list.length; i += 1) {
      if (list[i - 1] > list[i]) return; // unsorted input
    }
    this._array = list;
  }

  indexOf(element) {
    if (!this.array) return -1;
    return bsearch(this.array, element);
  }
}

module.exports = BinarySearch;
