// inspired by https://exercism.org/tracks/python/exercises/book-store/solutions/bobahop

var BookPrice = 800
var Discounted = [1.0, 1.0, 0.95, 0.90, 0.80, 0.75]

class BookStore {
  static total(basket) { this.new(basket).totalPrice }

  construct new(basket) {
    _bundles = [[]]
    basket.each {|book| addToBundle(book)}
  }

  totalPrice {
    optimizeBundles()

    return _bundles
      .map {|b| b.count}
      .reduce(0) {|sum, size| sum + size * BookPrice * Discounted[size]}
  }

  addToBundle(book) {
    for (bundle in _bundles) {
      if (!bundle.contains(book)) {
        bundle.add(book)
        return
      }
    }
    _bundles.add([book])
  }

  optimizeBundles() {
    // Two bundles of 4 are cheaper than a bundle of 5 plus a bundle of 3.
    // Look for a book in a 5-bundle that can be moved into a 3-bundle.

    var bundle5 = _bundles.where {|b| b.count == 5}
    if (bundle5.isEmpty) return

    var bundle3 = _bundles.where {|b| b.count == 3}
    if (bundle3.isEmpty) return

    var b5 = bundle5.toList[0]
    var b3 = bundle3.toList[0]

    for (book in b5) {
      if (!b3.contains(book)) {
        b3.add(book)
        b5.remove(book)
        break
      }
    }
    optimizeBundles()
  }
}
