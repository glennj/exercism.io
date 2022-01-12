export class Coord {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  equals(other) {
    return other instanceof Coord
        && this.x === other.x
        && this.y === other.y;
  }

  // for sorting
  cmp(other) {
    return (this.x - other.x) || (this.y - other.y);
  }
}

export class CoordsArray extends Array {
  includes(aCoord) {
    return !!this.find(coord => coord.equals(aCoord));
  }

  sort() {
    return super.sort((coord1, coord2) => coord1.cmp(coord2));
  }

  toPairsArray() {
    return this.sort().map(coord => [coord.x, coord.y]);
  }
}
