let seqId = 0;
const nextId = () => { seqId += 1; return seqId; };

/* *************************************************************** */
class Cell {
  constructor() {
    this.listeners = [];
    this.prev = null;
    this.value = null;
    this.id = nextId();
  }

  addListener(cell) {
    this.listeners.push(cell);
  }

  notifyListeners() {
    this.listeners.forEach(cell => cell.update());
  }
}

/* *************************************************************** */
class InputCell extends Cell {
  constructor(value) {
    super();
    this.setValue(value);
  }

  setValue(value) {
    this.prev = this.value;
    this.value = value;
    this.notifyListeners();
  }
}

/* *************************************************************** */
class ComputeCell extends Cell {
  constructor(cells, func) {
    super();
    this.cells = cells;
    this.func = func;
    this.numUpdatesReceived = 0;
    this.callbacks = new Map();
    this.value = func(cells);
    cells.forEach(cell => cell.addListener(this));
    this.notifyListeners(); // Danger Will Robinson! potential for update loops
  }

  update() {
    this.prev = this.value;
    this.value = this.func(this.cells);
    this.notifyListeners(); // Danger Will Robinson! potential for update loops

    // invoke callbacks if:
    // 1. I have recieved all my notifications
    // 2. my own value has changed
    // 3. the callback's computed value has changed.
    this.numUpdatesReceived += 1;
    if (this.numUpdatesReceived === this.cells.length) {
      this.numUpdatesReceived = 0;
      if (this.value !== this.prev) {
        this.callbacks.forEach((prev, cbCell) => {
          const value = cbCell.compute(this);
          if (value !== prev) {
            cbCell.update(value);
            this.callbacks.set(cbCell, value);
          }
        });
      }
    }
  }

  addCallback(cbCell) {
    // stash the current value for this callback
    this.callbacks.set(cbCell, cbCell.compute(this));
  }

  removeCallback(cell) {
    this.callbacks.delete(cell);
  }
}

/* *************************************************************** */
class CallbackCell extends Cell {
  constructor(func) {
    super();
    this.func = func;
    this.values = [];
  }

  compute(cell) {
    return this.func(cell);
  }

  update(value) {
    this.values.push(value);
  }
}

module.exports = { InputCell, ComputeCell, CallbackCell };
