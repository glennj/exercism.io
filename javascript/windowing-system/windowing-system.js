// @ts-check

export class Size {
  #width;
  #height;

  constructor(width, height) {
    this.resize(width, height);
  }

  get width()  { return this.#width; }
  get height() { return this.#height; }

  resize(width, height) {
    this.#width = width ?? 80;
    this.#height = height ?? 60;
  }

  clone() { return new Size(this.width, this.height); }
}


export class Position {
  #x;
  #y;

  constructor(x, y) {
    this.move(x, y);
  }

  get x() { return this.#x; }
  get y() { return this.#y; }

  move(x, y) {
    this.#x = x ?? 0;
    this.#y = y ?? 0;
  }

  clone() { return new Position(this.x, this.y); }
}


export class ProgramWindow {
  #size;
  #position;
  #screenSize;

  constructor() {
    this.#size       = new Size();
    this.#position   = new Position();
    this.#screenSize = new Size(800, 600);
  }

  get size()       { return this.#size.clone(); }
  get position()   { return this.#position.clone(); }
  get screenSize() { return this.#screenSize.clone(); }

  resize(newSize) {
    let w = Math.max(1, newSize.width);
    let h = Math.max(1, newSize.height);

    if (this.position.x + w > this.screenSize.width)
      w = this.screenSize.width - this.position.x;
    if (this.position.y + h > this.screenSize.height)
      h = this.screenSize.height - this.position.y;

    this.#size = new Size(w, h);
  }

  move(newPosition) {
    let x = Math.max(0, newPosition.x);
    let y = Math.max(0, newPosition.y);

    if (x + this.size.width > this.screenSize.width)
      x = this.screenSize.width - this.size.width;
    if (y + this.size.height > this.screenSize.height)
      y = this.screenSize.height - this.size.height;

    this.#position = new Position(x, y);
  }
}


const CHANGE_SIZE = new Size(400, 300);
const CHANGE_POS  = new Position(100, 150);

export const changeWindow = function(window) {
  window.resize(CHANGE_SIZE);
  window.move(CHANGE_POS);
  return window;
};
