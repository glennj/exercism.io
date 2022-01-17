class BufferFullException(BufferError):
    pass

class BufferEmptyException(BufferError):
    pass


class CircularBuffer(object):
    def __init__(self, capacity):
        self.header = Header(capacity)
        self.buffer = [None] * capacity

    def is_empty(self):
        return self.buffer[self.header.read] is None

    def is_full(self):
        return self.header.read == self.header.write \
            and not self.is_empty()

    def read(self):
        if self.is_empty():
            raise BufferEmptyException('Circular buffer is empty')
        data = self.buffer[self.header.read]
        self.buffer[self.header.read] = None
        self.header.inc_r()
        return data

    def write(self, data):
        if self.is_full():
            raise BufferFullException('Circular buffer is full')
        self.buffer[self.header.write] = data
        self.header.inc_w()
        return self

    def overwrite(self, data):
        if self.is_full():
            self.read()
        return self.write(data)

    def clear(self):
        self.buffer = [None] * len(self.buffer)
        self.header.reset()
        return self


class Header(object):
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

    def inc_r(self, header='r'):
        self.p[header] = (self.p[header] + 1) % self.cap

    def inc_w(self):
        self.inc_r('w')
