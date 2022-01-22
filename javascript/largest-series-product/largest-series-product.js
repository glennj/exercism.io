import {ExtendedArray} from './extended-array';

class Series {
  constructor(input) {
    if (/\D/.test(input))
      throw new Error('Digits input must only contain digits');
    this.digits = ExtendedArray.from(input).map(n => parseInt(n, 10));
  }

  largestProduct(span) {
    if (span < 0)
      throw new Error('Span must be greater than zero');
    if (span > this.digits.length)
      throw new Error('Span must be smaller than string length');

    const products = [];
    for (const tuple of this.digits.eachConsecutive(span)) {
      products.push(tuple.product());
    }
    return Math.max(...products);
  }
}

export function largestProduct(series, span) {
  return new Series(series).largestProduct(span);
}
