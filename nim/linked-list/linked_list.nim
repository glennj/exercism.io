import std/options
import std/sugar

#[  This is LinkedList implemented as a node.
    To find the last node (and the length), I need to walk the list.
    This is slower, but it's an experiment.

    I've also implemented a `forEach` method.
    I use that to find the length, the last node, and for deleting.
]#

type
  LinkedList*[T] = ref object
    next, prev: LinkedList[T]
    value: Option[T]

# Utility methods

proc isEmpty[T](list: LinkedList[T]): bool = list.value.isNone
proc hasNext[T](list: LinkedList[T]): bool = list.next != nil
proc hasPrev[T](list: LinkedList[T]): bool = list.prev != nil

proc lastNode[T](list: LinkedList[T]): LinkedList[T] =
  var last = list
  list.forEach((node: LinkedList[T]) => (last = node))
  return last

# Adding values to the list

proc push*[T](list: var LinkedList[T], val: T) =
  if list.isEmpty:
    list.value = some(val)
  else:
    var last = list.lastNode
    var newList = LinkedList[T]()
    last.next = newList
    newList.prev = last
    newList.value = some(val)

proc unshift*[T](list: var LinkedList[T], val: T) =
  if list.isEmpty:
    list.value = some(val)
  else:
    var newList = LinkedList[T]()
    newList.value = some(val)
    newList.next = list
    list.prev = newList
    list = newList

# Extracting values from the list

proc pop*[T](list: var LinkedList[T]): T =
  var node = list.lastNode
  result = node.value.get
  if node.hasPrev:
    node.prev.next = nil
    node.prev = nil
  else:
    node.value = none(T)

proc shift*[T](list: var LinkedList[T]): T =
  result = list.value.get
  if list.hasNext:
    list.next.prev = nil
    list = list.next
  else:
    list.value = none(T)

# Deleting values from the list

proc delete*[T](list: var LinkedList[T], val: T) =
  var foundIt = false
  var newList = LinkedList[T]()
  list.forEach((node: LinkedList[T]) => (
    if node.value == some(val) and not foundIt:
      foundIt = true
    else:
      newList.push(node.value.get)
  ))
  list = newList

# List size

proc len*[T](list: LinkedList[T]): int =
  var count = 0
  list.forEach((node: LinkedList[T]) => (count = count + 1))
  return count

# Iterating over the list

proc forEach*[T](list: LinkedList[T], callback: (lst: LinkedList[T]) -> void) =
  if list.isEmpty: return
  var node = list
  while true:
    callback(node)
    if not node.hasNext: break
    node = node.next
