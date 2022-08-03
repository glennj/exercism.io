import "wren-testie/testie" for Testie, Expect
import "./linked-list" for LinkedList

Testie.test("Linked List") { |do, skip|

  do.test("pop gets last element from the list") {
    var list = LinkedList.new()
    list.push(7)
    Expect.value(list.pop()).toEqual(7)
  }

  do.test("push/pop add and remove at the end of the list respectively") {
    var list = LinkedList.new()
    list.push(11)
    list.push(13)
    Expect.value(list.pop()).toEqual(13)
    Expect.value(list.pop()).toEqual(11)
  }

  do.test("shift gets only element in the list") {
    var list = LinkedList.new()
    list.push(17)
    Expect.value(list.shift()).toEqual(17)
  }

  do.test("shift gets elements from the beginning of the list") {
    var list = LinkedList.new()
    list.push(23)
    list.push(5)
    Expect.value(list.shift()).toEqual(23)
    Expect.value(list.shift()).toEqual(5)
  }
  
  do.test("unshift adds element to beginning of the list") {
    var list = LinkedList.new()
    list.unshift(23)
    list.unshift(5)
    Expect.value(list.shift()).toEqual(5)
    Expect.value(list.shift()).toEqual(23)
  }

  do.test("pop, push, shift, and unshift can be used in any order") {
    var list = LinkedList.new()
    list.push(1)
    list.push(2)
    Expect.value(list.pop()).toEqual(2)

    list.push(3)
    Expect.value(list.shift()).toEqual(1)

    list.unshift(4)
    list.push(5)
    Expect.value(list.shift()).toEqual(4)
    Expect.value(list.pop()).toEqual(5)
    Expect.value(list.shift()).toEqual(3)
  }

  do.test("count an empty list") {
    var list = LinkedList.new()
    Expect.value(list.count).toEqual(0)
  }


  do.test("count a list with items") {
    var list = LinkedList.new()
    list.push(37)
    list.push(1)
    Expect.value(list.count).toEqual(2)
  }

  do.test("count is correct after mutation") {
    var list = LinkedList.new()
    list.push(31)
    Expect.value(list.count).toEqual(1)
    list.unshift(43)
    Expect.value(list.count).toEqual(2)
    list.shift()
    Expect.value(list.count).toEqual(1)
    list.pop()
    Expect.value(list.count).toEqual(0)
  }

  do.test("shift gets only element in the list") {
    var list = LinkedList.new()
    list.push(41)
    list.push(59)
    list.pop()
    list.pop()
    list.push(47)
    Expect.value(list.count).toEqual(1)
    Expect.value(list.pop()).toEqual(47)
  }

  do.test("shifting to empty doesn't break the list") {
    var list = LinkedList.new()
    list.push(41)
    list.push(59)
    list.shift()
    list.shift()
    list.push(47)
    Expect.value(list.count).toEqual(1)
    Expect.value(list.shift()).toEqual(47)
  }

  do.test("push/pop add and remove at the end of the list respectively") {
    var list = LinkedList.new()
    list.push(61)
    list.delete(61)
    Expect.value(list.count).toEqual(0)
  }

  do.test("deletes the element with the specified value from the list") {
    var list = LinkedList.new()
    list.push(71)
    list.push(83)
    list.push(79)

    list.delete(83)

    Expect.value(list.count).toEqual(2)
    Expect.value(list.pop()).toEqual(79)
    Expect.value(list.shift()).toEqual(71)
  }

  do.test("deletes the element with the specified value from the list, re-assigns tail") {
    var list = LinkedList.new()
    list.push(71)
    list.push(83)
    list.push(79)

    list.delete(83)

    Expect.value(list.count).toEqual(2)
    Expect.value(list.pop()).toEqual(79)
    Expect.value(list.pop()).toEqual(71)
  }

  do.test("deletes the element with the specified value from the list, re-assigns head") {
    var list = LinkedList.new()
    list.push(71)
    list.push(83)
    list.push(79)

    list.delete(83)

    Expect.value(list.count).toEqual(2)
    Expect.value(list.shift()).toEqual(71)
    Expect.value(list.shift()).toEqual(79)
  }

  do.test("deletes the first of two elements") {
    var list = LinkedList.new()
    list.push(97)
    list.push(101)

    list.delete(97)

    Expect.value(list.count).toEqual(1)
    Expect.value(list.pop()).toEqual(101)
  }

  do.test("deletes the second of two elements") {
    var list = LinkedList.new()
    list.push(97)
    list.push(101)

    list.delete(101)

    Expect.value(list.count).toEqual(1)
    Expect.value(list.pop()).toEqual(97)
  }

  do.test("delete does not modify the list if the element is not found") {
    var list = LinkedList.new()
    list.push(89)
    list.delete(103)

    Expect.value(list.count).toEqual(1)
  }

  do.test("deletes only the first occurrence") {
    var list = LinkedList.new()
    list.push(73)
    list.push(9)
    list.push(9)
    list.push(107)

    list.delete(9)

    Expect.value(list.count).toEqual(3)

    Expect.value(list.pop()).toEqual(107)
    Expect.value(list.pop()).toEqual(9)
    Expect.value(list.pop()).toEqual(73)
  }

  // bonus test: uncomment for extra credit
  /*
  do.test("iteration") {
    var list = LinkedList.new()
    list.push(10)
    list.push(20)
    list.push(30)
    list.push(40)
    var result = []
    for (value in list) {
      result.add(value / 2)
    }
    Expect.value(result).toEqual([5, 10, 15, 20])
  }
  */
}

