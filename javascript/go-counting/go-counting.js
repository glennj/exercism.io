import {Coord, CoordsArray} from './coords'

const STONES = ['B', 'W'];
const EMPTY = ' ';
const PLAYERS = {'B': 'BLACK', 'W': 'WHITE', EMPTY: 'NONE'};

class Stack extends Array {
  isEmpty() { return this.length === 0; }
}

//--------------------------------------------------------
export class GoCounting {
  #board;
  #height;
  #width;

  constructor(board) {
    this.#board = board.map(row => row.split(''));
    this.#height = this.#board.length;
    this.#width = this.#board[0].length;
  }

  getTerritory(x, y) {
    const result = this.#getTerritory(new Coord(x, y));
    if (!result.error) {
      result.territory = result.territory.toPairsArray();
    }
    return result;
  }

  getTerritories() {
    const territories = {};
    for (const stone in PLAYERS) {
      territories[PLAYERS[stone]] = new CoordsArray();
    }

    const seen = coord => {
      for (const stone in PLAYERS) {
        if (territories[PLAYERS[stone]].includes(coord))
          return true;
      }
      return false;
    };

    for (let x = 0; x < this.#width; x++) {
      for (let y = 0; y < this.#height; y++) {
        const coord = new Coord(x, y);
        if (this.#at(coord) === EMPTY && !seen(coord)) {
          const {owner, territory} = this.#getTerritory(coord);
          territories[owner].push(...territory);
        }
      }
    }

    return {
      territoryBlack: territories.BLACK.toPairsArray(),
      territoryWhite: territories.WHITE.toPairsArray(),
      territoryNone:  territories.NONE.toPairsArray(),
    };
  }

  //--------------------------------------------------------
  // this private method returns the territory as an array of Coords
  #getTerritory(coord) {
    if (!this.#isOnBoard(coord))
      return {error: 'Invalid coordinate'};

    let owner = 'NONE';
    const territory = new CoordsArray();

    if (this.#at(coord) === EMPTY) {
      const visited = new CoordsArray();
      const border  = new Array();
      const toVisit = new Stack();

      toVisit.push(coord);

      while (!toVisit.isEmpty()) {
        coord = toVisit.pop();
        const stone = this.#at(coord);
        if (stone !== EMPTY) {
          border.push(stone);
        }
        else {
          visited.push(coord);
          territory.push(coord);

          this.#adjacentCoords(coord)
            .filter(neighbour => !(visited.includes(neighbour) || territory.includes(neighbour)))
            .forEach(neighbour => toVisit.push(neighbour));
        }
      }

      if (border.length > 0) {
        const ownerStone = border.pop();
        if (border.every(stone => stone === ownerStone))
          owner = PLAYERS[ownerStone];
      }
    }

    return {owner, territory};
  }

  //--------------------------------------------------------

  #at(coord) {
    return this.#board[coord.y][coord.x];
  }

  #isOnBoard(coord) {
    return 0 <= coord.x && coord.x < this.#width
        && 0 <= coord.y && coord.y < this.#height
  }

  #adjacentCoords(coord) {
    const neighbours = [
      new Coord(coord.x-1, coord.y),
      new Coord(coord.x+1, coord.y),
      new Coord(coord.x, coord.y-1),
      new Coord(coord.x, coord.y+1),
    ];
    return neighbours.filter(coord => this.#isOnBoard(coord));
  }
}
