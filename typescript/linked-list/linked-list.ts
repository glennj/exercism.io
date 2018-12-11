class Node<T> {
  value: T
  next: Node<T> | undefined
  prev: Node<T> | undefined
  constructor(value: T) { this.value = value }
}

class LinkedList<T> {
  head: Node<T> | undefined
  tail: Node<T> | undefined

  count(): number {
    let count = 0
    let node = this.head
    while (node !== undefined) {
      node = node.next
      count += 1
    }
    return count
  }

  push(value: T): LinkedList<T> {
    const node = new Node<T>(value)
    if (this.tail === undefined) {
      this.head = node
    } else {
      this.tail.next = node
      node.prev = this.tail
    }
    this.tail = node
    return this
  }

  unshift(value: T): LinkedList<T> {
    const node = new Node<T>(value)
    if (this.head === undefined) {
      this.tail = node
    } else {
      this.head.prev = node
      node.next = this.head
    }
    this.head = node
    return this
  }

  pop(): T {
    const node = this.tail
    if (node === undefined) {
      throw new Error('List is empty')
    }
    if (node.prev === undefined) {
      this.tail = undefined
      this.head = undefined
    } else {
      node.prev.next = undefined
      this.tail = node.prev
    }
    return node.value
  }

  shift(): T {
    const node = this.head
    if (node === undefined) {
      throw new Error('List is empty')
    }
    if (node.next === undefined) {
      this.tail = undefined
      this.head = undefined
    } else {
      node.next.prev = undefined
      this.head = node.next
    }
    return node.value
  }

  delete(value: T): LinkedList<T> {
    let node = this.head
    while (node !== undefined) {
      if (node.value !== value) {
        node = node.next
        continue
      }

      if (node.next) {
        node.next.prev = node.prev
      } else {
        this.tail = node.prev
      }

      if (node.prev) {
        node.prev.next = node.next
      } else {
        this.head = node.next
      }

      break
    }
    return this
  }
}

export default LinkedList
