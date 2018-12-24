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

/* community
 *
 * `isSorted` check

      function isSorted(array) {
          return array.every(function(n, i, a) {
              return i == 0 || n >= a[i - 1]
          })
      }

 *
 * make `indexOf` itself recursive
 *
 * or, non-recursive

      indexOf(item) {
        let mid
        ,   left  = 0
        ,   right = this.array.length;

        while (right > left) {
          mid = Math.floor((left+right)/2);
          if (this.array[mid] === item) return mid;

          [left, right] = (item > this.array[mid])
                        ? [mid+1, right]
                        : [left, mid-1];
        }
        return (this.array[right] === item) ? right : -1;
      }
 *
 */
