import {ExtendedArray} from './extended-array';

export class List {
  constructor(a = []) {
    this.a = ExtendedArray.from(a);
  }

  compare(other) {
    if (this.a.length === other.a.length && this.a.equals(other.a))
      return 'EQUAL';

    if (this.a.length > other.a.length && this.a.containsArray(other.a))
      return 'SUPERLIST';

    if (this.a.length < other.a.length && other.a.containsArray(this.a))
      return 'SUBLIST';

    return 'UNEQUAL';
  }
}
