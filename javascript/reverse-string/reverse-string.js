import {from} from './iterable-range';

export const reverseString = (str) => 
  from(str.length - 1)
    .downTo(0)
    .map(i => str[i])
    .join('');
