/* eslint no-underscore-dangle: ["error", { "allowAfterThis": true }] */
/* eslint-disable  no-param-reassign */

class ElementValueRequiredException extends Error {}
class ElementNextNotInstanceException extends Error {}

class Element {
  constructor(value, element) {
    if (value === undefined || value === null) {
      throw new ElementValueRequiredException();
    }
    this.value = value;
    this.next = element;
  }

  get next() { return this._next; }

  set next(element) {
    if (element === undefined || element instanceof Element) {
      this._next = element;
    } else {
      throw new ElementNextNotInstanceException();
    }
  }
}

class List {
  forEach(func) {
    let node = this.head;
    while (node) {
      func(node);
      node = node.next;
    }
  }

  push(element) {
    if (!(element instanceof Element)) throw new Error();
    let last;
    this.forEach((node) => { last = node; });
    if (last) {
      last.next = element;
    } else {
      this.head = element;
    }
  }

  pop() {
    let last;
    let prev;
    this.forEach((node) => { last = node; if (node.next) prev = node; });
    if (last) this.head = prev;
    if (prev) prev.next = undefined;
    return last;
  }

  unshift(element) {
    if (!(element instanceof Element)) throw new Error();
    element.next = this.head;
    this.head = element;
  }

  shift() {
    const node = this.head;
    if (node) this.head = node.next;
    return node;
  }

  static fromArray(a) {
    const list = new List();
    // more efficient to `unshift` than `push`
    a.reverse().forEach(value => list.unshift(new Element(value)));
    return list;
  }

  toArray() {
    const a = [];
    this.forEach(node => a.push(node.value));
    return a;
  }

  reverse() {
    const r = new List();
    this.forEach(node => r.unshift(new Element(node.value)));
    this.head = r.head;
  }
}

module.exports = {
  List,
  Element,
  ElementValueRequiredException,
  ElementNextNotInstanceException,
};
