class CustomSet(object):

    def __init__(self, elements=[]):
        self._items = {}
        for elem in elements:
            self.add(elem)

    def __len__(self):
        return len(self._items)

    def __iter__(self):
        return iter(self._items)

    def isempty(self):
        return len(self) == 0

    def __contains__(self, element):
        return element in self._items

    def issubset(self, other):
        if self.isempty():
            return True
        if len(self) > len(other):
            return False
        return all(item in other for item in self)

    def isdisjoint(self, other):
        if self.isempty() or other.isempty():
            return True
        return all(item not in other for item in self)

    def __eq__(self, other):
        return self.issubset(other) and other.issubset(self)

    def add(self, element):
        self._items[element] = True

    def intersection(self, other):
        return CustomSet([elem for elem in self if elem in other])

    def __sub__(self, other):
        return CustomSet([elem for elem in self if elem not in other])

    def __add__(self, other):
        return CustomSet(
            [elem for elem in self] +
            [elem for elem in other]
        )
