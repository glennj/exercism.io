class Node {
  constructor(value, nextNode = null, previousNode = null) {
    this.value = value;
    this.next = nextNode;
    this.prev = previousNode;
  }
}

class LinkedList {
  constructor() {
    this.head = null;
    this.tail = null;
  }

  push(value) {
    const node = new Node(value);
    if (this.tail === null) {
      this.head = node;
    } else {
      this.tail.next = node;
      node.prev = this.tail;
    }
    this.tail = node;
    return this;
  }

  unshift(value) {
    const node = new Node(value);
    if (this.head === null) {
      this.tail = node;
    } else {
      this.head.prev = node;
      node.next = this.head;
    }
    this.head = node;
    return this;
  }

  pop() {
    if (this.tail === null) return null;
    const node = this.tail;
    if (node.prev === null) {
      this.head = null;
      this.tail = null;
    } else {
      node.prev.next = null;
      this.tail = node.prev;
    }
    return node.value;
  }

  shift() {
    if (this.head === null) return null;
    const node = this.head;
    if (node.next === null) {
      this.head = null;
      this.tail = null;
    } else {
      node.next.prev = null;
      this.head = node.next;
    }
    return node.value;
  }

  delete(value) {
    let node = this.head;
    while (node != null) {
      if (node.value !== value) {
        node = node.next;
      } else {
        if (node.prev === null) {
          this.head = node.next;
        } else {
          node.prev.next = node.next;
        }
        if (node.next === null) {
          this.tail = node.prev;
        } else {
          node.next.prev = node.prev;
        }
        break;
      }
    }
    return this;
  }

  // walk the list
  values() {
    const values = [];
    let node = this.head;
    while (node != null) {
      values.push(node.value);
      node = node.next;
    }
    return values;
  }

  forEach(callback) {
    this.values().forEach(callback);
  }

  count() {
    return this.values().length;
  }

  toString() {
    return this.values().join(',');
  }
}

module.exports = LinkedList;
