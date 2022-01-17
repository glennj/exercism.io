class Node(object):
    def __init__(self, value, next_node=None):
        self._value = value
        self._next = next_node

    def value(self):
        return self._value

    def next(self):
        return self._next


class LinkedList(object):
    def __init__(self, values=[]):
        self._head = None
        for val in values:
            self.push(val)

    def each(self):
        node = self._head
        while node:
            yield node
            node = node.next()

    def reduce(self, func, acc):
        for node in self.each():
            acc = func(acc, node)
        return acc

    def __iter__(self):
        for node in self.each():
            yield node.value()

    def __len__(self):
        return self.reduce(lambda acc, _: acc + 1, 0)

    def head(self):
        if self._head is None:
            raise EmptyListException("The list is empty.")
        return self._head

    def push(self, value):
        node = Node(value, self._head)
        self._head = node
        return self

    def pop(self):
        if self._head is None:
            raise EmptyListException("The list is empty.")

        val = self._head.value()
        self._head = self._head.next()
        return val

    def reversed(self):
        return self.reduce(
            lambda rev, node: rev.push(node.value()),
            LinkedList()
        )


class EmptyListException(Exception):
    pass
