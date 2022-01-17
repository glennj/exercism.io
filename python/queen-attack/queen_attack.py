class Queen(object):
    def __init__(self, row, column):
        if row < 0: raise ValueError('row not positive')
        if row > 7: raise ValueError('row not on board')
        if column < 0: raise ValueError('column not positive')
        if column > 7: raise ValueError('column not on board')
        self.x = row
        self.y = column

    def can_attack(self, other):
        if self.x == other.x and self.y == other.y:
            raise ValueError('Invalid queen position: both queens in the same square')
        dx = abs(self.x - other.x)
        dy = abs(self.y - other.y)
        return dx == 0 or dy == 0 or dx == dy
