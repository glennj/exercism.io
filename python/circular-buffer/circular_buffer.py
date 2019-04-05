class BufferFullException(Exception):
    pass


class BufferEmptyException(Exception):
    pass


class CircularBuffer(object):
    def __init__(self, capacity):
        self.pointer = Pointer(capacity)
        self.buffer = [None] * capacity

    def is_empty(self):
        return self.buffer[self.pointer.read] is None

    def is_full(self):
        return self.pointer.read == self.pointer.write \
            and not self.is_empty()

    def read(self):
        if self.is_empty():
            raise BufferEmptyException('empty')
        data = self.buffer[self.pointer.read]
        self.buffer[self.pointer.read] = None
        self.pointer.inc_r()
        return data

    def write(self, data):
        if self.is_full():
            raise BufferFullException('full')
        self.buffer[self.pointer.write] = data
        self.pointer.inc_w()
        return self

    def overwrite(self, data):
        if self.is_full():
            self.read()
        return self.write(data)

    def clear(self):
        self.buffer = [None] * len(self.buffer)
        self.pointer.reset()
        return self


class Pointer(object):
    def __init__(self, capacity):
        self.cap = capacity
        self.reset()

    def reset(self):
        self.p = {'r': 0, 'w': 0}

    @property
    def read(self):
        return self.p['r']

    @property
    def write(self):
        return self.p['w']

    def inc_r(self, pointer='r'):
        self.p[pointer] = (self.p[pointer] + 1) % self.cap

    def inc_w(self):
        self.inc_r('w')
