export class Element {
  constructor(value) {
    this.value = value;
    this.next = null;
  }
}

export class List {
  constructor(initialValues = []) {
    this.head = null;
    initialValues.forEach(value => this.add(new Element(value)));
  }

  get length() {
    let len = 0;
    this.forEach(() => len++);
    return len;
  }

  add(element) {
    element.next = this.head;
    this.head = element;
  }

  forEach(callback) {
    let node = this.head;
    while (node) {
      callback(node);
      node = node.next;
    }
  }

  toArray() {
    const a = [];
    this.forEach(node => a.push(node.value));
    return a;
  }

  reverse() {
    const rev = new List();
    this.forEach(node => rev.add(new Element(node.value)));
    return rev;
  }
}
