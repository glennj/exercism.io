import math


def measure(bucket_one, bucket_two, goal, start_bucket):
    if goal > bucket_one + bucket_two:
        raise ValueError('Unsatisfiable goal.')
    gcd = math.gcd(bucket_one, bucket_two)
    if gcd > 1 and goal % gcd > 0:
        raise ValueError('Unsatisfiable goal.')
    if start_bucket not in ('one', 'two'):
        raise ValueError('Unknown bucket.')

    one = Bucket('one', bucket_one)
    two = Bucket('two', bucket_two)
    start, other = (one, two) if start_bucket == 'one' else (two, one)

    return start.solve(other, goal)


class Bucket:
    def __init__(self, name, size):
        self.name = name
        self.size = size
        self.empty()

    @property
    def capacity(self):
        return self.size - self.amount

    def empty(self):
        self.amount = 0

    def is_empty(self):
        return self.amount == 0

    def fill(self):
        self.amount = self.size

    def is_full(self):
        return self.amount == self.size

    def pour(self, other):
        to_pour = min(self.amount, other.capacity)
        self.amount -= to_pour
        other.amount += to_pour

    def solve(self, other, goal):
        self.fill()
        moves = 1
        if other.size == goal:
            other.fill()
            moves += 1

        while True:
            if self.amount == goal:
                return moves, self.name, other.amount
            if other.amount == goal:
                return moves, other.name, self.amount

            if self.is_empty():
                self.fill()
            elif other.is_full():
                other.empty()
            else:
                self.pour(other)
            moves += 1
