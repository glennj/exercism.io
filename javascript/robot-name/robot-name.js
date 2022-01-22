import {from} from './iterable-range';

// construct all the names up front
const NAMES = [];
for (const i of from(65).upTo(90)) {
  const l1 = String.fromCharCode(i);
  for (const j of from(65).upTo(90)) {
    const l2 = String.fromCharCode(j);
    for (const n of from(0).upTo(999)) {
      NAMES.push(l1 + l2 + n.toString().padStart(3, '0'));
    }
  }
}

function shuffleNames() {
  NAMES.sort(() => Math.random() - 0.5);
}
shuffleNames();

let nameIndex = 0;


export class Robot {
  #name;

  static releaseNames() {
    nameIndex = 0;
    shuffleNames();
  }

  constructor() {
    this.reset();
  }

  reset() {
    if (nameIndex === NAMES.length) {
      throw new Error('No more names');
    }
    this.#name = NAMES[nameIndex++];
  }

  get name() {
    return this.#name;
  }
  set name(str) {
    throw new Error('names cannot be modified');
  }
}
