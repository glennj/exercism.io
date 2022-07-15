import "./simple-linked-list" for LinkedList, Element
import "wren-testie/testie" for Testie, Expect

Testie.test("Simple Linked List") { |do, skip|
  do.describe("Element class") {
    do.test("has constructor") {
      var element = Element.new(1)
      Expect.value(element.value).toEqual(1)
    }

    skip.test("value reflects constructor arg") {
      var element = Element.new(2)
      Expect.value(element.value).toEqual(2)
    }

    skip.test("has null for next by default") {
      var element = Element.new(1)
      Expect.value(element.next).toEqual(null)
    }
  }

  do.describe("List class") {
    skip.test("has constructor") {
      var list = LinkedList.new()
      Expect.value(list).toBeDefined()
    }

    skip.test("list.news should have count 0") {
      var list = LinkedList.new()
      Expect.value(list.count).toEqual(0)
    }

    skip.test("can add a element") {
      var list = LinkedList.new()
      var element = Element.new(1)
      Expect.that { list.add(element) }.toNotAbort()
    }

    skip.test("adding a element increments count") {
      var list = LinkedList.new()
      var element = Element.new(1)
      list.add(element)
      Expect.value(list.count).toEqual(1)
    }

    skip.test("adding two elements increments twice") {
      var list = LinkedList.new()
      var element1 = Element.new(1)
      var element2 = Element.new(3)
      list.add(element1)
      list.add(element2)
      Expect.value(list.count).toEqual(2)
    }

    skip.test("List.news have a null head element") {
      var list = LinkedList.new()
      Expect.value(list.head).toEqual(null)
    }

    skip.test("adding an Element to an empty list sets the head Element") {
      var list = LinkedList.new()
      var element = Element.new(1)
      list.add(element)
      Expect.value(list.head.value).toEqual(1)
    }

    skip.test("adding a second Element updates the head Element") {
      var list = LinkedList.new()
      var element1 = Element.new(1)
      var element2 = Element.new(3)
      list.add(element1)
      list.add(element2)
      Expect.value(list.head.value).toEqual(3)
    }

    skip.test("can get the next Element from the head") {
      var list = LinkedList.new()
      var element1 = Element.new(1)
      var element2 = Element.new(3)
      list.add(element1)
      list.add(element2)
      Expect.value(list.head.next.value).toEqual(1)
    }

    skip.test("can be initialized with an array") {
      var list = LinkedList.new([1, 2, 3])
      Expect.value(list.count).toEqual(3)
      Expect.value(list.head.value).toEqual(3)
    }
  }

  do.describe("Lists with multiple elements") {
    var list
    do.beforeEach {
      list = LinkedList.new([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    }

    skip.test("with correct length") {
      Expect.value(list.count).toEqual(10)
    }

    skip.test("with correct head value") {
      Expect.value(list.head.value).toEqual(10)
    }

    skip.test("can traverse the list") {
      Expect.value(list.head.next.next.next.value).toEqual(7)
    }

    skip.test("can convert to an array") {
      var oneList = LinkedList.new([1])
      Expect.value(oneList.toList).toEqual([1])
    }

    skip.test("head of list is final element from input array") {
      var twoList = LinkedList.new([1, 2])
      Expect.value(twoList.head.value).toEqual(2)
    }

    skip.test("can convert to an array") {
      var oneList = LinkedList.new([1])
      Expect.value(oneList.toList).toEqual([1])
    }

    skip.test("can convert longer list to an array") {
      Expect.value(list.toList).toEqual([10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
    }

    skip.test("can be reversed") {
      var twoList = LinkedList.new([1, 2])
      Expect.value(twoList.reverse().toList).toEqual([1, 2])
    }

    skip.test("can be reversed when it has more elements") {
      var threeList = LinkedList.new([1, 2, 3])
      Expect.value(threeList.reverse().toList).toEqual([1, 2, 3])
    }

    skip.test("can reverse with many elements") {
      Expect.value(list.reverse().toList).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    }

    skip.test("can reverse a reversal") {
      Expect.value(list.reverse().reverse().toList).toEqual([
        10,
        9,
        8,
        7,
        6,
        5,
        4,
        3,
        2,
        1,
      ])
    }

    skip.test("can be treated as iterable") {
      var items = []
      for (item in list) {
        items.add(item)
      }
      Expect.value(items).toEqual([10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
    }
  }
}
