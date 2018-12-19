class Zipper {
  constructor(dataStructure = null, focus = dataStructure, previous = null) {
    this.dataStructure = dataStructure;
    this.focus = focus;
    this.previous = previous;
  }

  static fromTree(t) {
    const zipper = new Zipper(t);
    return zipper;
  }
  clone() { return new Zipper(this.dataStructure, this.focus, this.previous); }

  toTree() { return this.dataStructure; }

  value() { return this.focus.value; }

  fromFocusProp(prop) { 
    const prev = this.clone();
    if (!this.focus[prop]) return null;
    return new Zipper(this.dataStructure, this.focus[prop], prev);
  }

  left() { return this.fromFocusProp('left'); }
  right() { return this.fromFocusProp('right'); }

  up() {
    return this.previous;
  }

  setValue(value) {
    debugger;
    this.focus.value = value;
    return this.clone();
  }
}

module.exports = Zipper;



function bt(value, left, right) {
  return {
    value,
    left,
    right,
  };
}

function leaf(value) {
  return bt(value, null, null);
}

  const t1 = bt(1, bt(2, null, leaf(3)), leaf(4));
  const t2 = bt(1, bt(5, null, leaf(3)), leaf(4));
  const t3 = bt(1, bt(2, leaf(5), leaf(3)), leaf(4));
  const t4 = bt(1, leaf(2), leaf(4));
  const t5 = bt(1, bt(2, null, leaf(3)), bt(6, leaf(7), leaf(8)));
  const t6 = bt(1, bt(2, null, leaf(5)), leaf(4));
  let zipper;

    zipper = Zipper.fromTree(t1);

    console.log(zipper.left().right().value()); //.toEqual(3);
debugger;
    console.log(zipper.left().right().setValue(5).toTree()); //.toEqual(t6);

