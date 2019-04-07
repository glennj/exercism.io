class Node(object):
    def __init__(self, value, succeeding=None, previous=None):
        self.value = value
        self.succ = succeeding
        self.prev = previous


class LinkedList(object):
    def __init__(self):
        self.head = None
        self.tail = None

    def push(self, value):
        node = Node(value)
        if self.tail:
            self.tail.succ = node
            node.prev = self.tail
        else:
            self.head = node
        self.tail = node

    def pop(self):
        if self.tail is None:
            raise ValueError('list is empty')
        node = self.tail
        if node.prev:
            node.prev.succ = None
            self.tail = node.prev
        else:
            self.tail = None
            self.head = None
        return node.value

    def unshift(self, value):
        node = Node(value)
        if self.head:
            self.head.prev = node
            node.succ = self.head
        else:
            self.tail = node
        self.head = node

    def shift(self):
        if self.head is None:
            raise ValueError('list is empty')
        node = self.head
        if node.succ:
            node.succ.prev = None
            self.head = node.succ
        else:
            self.head = None
            self.tail = None
        return node.value

    def __len__(self):
        length = 0
        node = self.head
        while node is not None:
            length += 1
            node = node.succ
        return length

    def __iter__(self):
        node = self.head
        while node is not None:
            yield node.value
            node = node.succ
