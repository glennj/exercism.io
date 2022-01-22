import {ExtendedArray} from './extended-array';

export class Series {
  constructor(str) {
    this.digits = ExtendedArray.from(
      str.match(/\d/g)?.map(d => Number.parseInt(d, 10)) ?? []
    );
  }

  slices(n) {
    if (this.digits.length === 0)
      throw new Error('series cannot be empty');
    if (n > this.digits.length)
      throw new Error('slice length cannot be greater than series length');
    if (n === 0)
      throw new Error('slice length cannot be zero');
    if (n < 0)
      throw new Error('slice length cannot be negative');

    const result = [];
    for (const sublist of this.digits.eachConsecutive(n))
      result.push(sublist.toArray());
    return result;
  }
}
