class Queen(object):
    def __init__(self, row, column):
        if (not 0 <= row <= 7) or (not 0 <= column <= 7):
            raise ValueError('Invalid position')
        self.x = row
        self.y = column

    def can_attack(self, other):
        if self.x == other.x and self.y == other.y:
            raise ValueError('Cannot occupy same position')
        dx = abs(self.x - other.x)
        dy = abs(self.y - other.y)
        return dx == 0 or dy == 0 or dx == dy
