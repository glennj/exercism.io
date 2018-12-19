export default class BinarySearch {
  readonly array: number[]

  constructor(list: number[]) {
    if (list.every((v, i) => i === 0 || list[i - 1] <= v)) {
      this.array = list
    }
  }

  indexOf(val: number, i: number = 0, j: number = this.array!.length - 1): number {
    if (i > j) { return -1 }
    const mid = i + Math.ceil((j - i) / 2)
    if (val === this.array[mid]) { return mid }
    if (val  <  this.array[mid]) { j = mid - 1 }
    if (val  >  this.array[mid]) { i = mid + 1 }
    return this.indexOf(val, i, j)
  }
}
