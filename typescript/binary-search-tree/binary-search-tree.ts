export class BinarySearchTree {
  private _left: BinarySearchTree|null = null
  private _right: BinarySearchTree|null = null
  // don't allow modification
  readonly data: number

  constructor(value: number) { this.data = value }

  // no setters
  get left():  BinarySearchTree|null { return this._left }
  get right(): BinarySearchTree|null { return this._right }

  each(callback: (element: number) => void): void {
    if (this.left)  { this.left.each(callback) }
    callback(this.data)
    if (this.right) { this.right.each(callback) }
  }

  insert(value: number): void {
    if (value <= this.data) {
      if (this.left) {
        this.left.insert(value)
      } else {
        this._left = new BinarySearchTree(value)
      }
    } else {
      if (this.right) {
        this.right.insert(value)
      } else {
        this._right = new BinarySearchTree(value)
      }
    }
  }
}
