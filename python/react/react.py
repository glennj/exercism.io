class Cell(object):
    def __init__(self, value=None):
        self._value = value
        self._listeners = []

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, val):
        self._value = val

    def add_listener(self, cell):
        self._listeners.append(cell)

    def recompute_listeners(self):
        for listener in self._listeners:
            listener.recompute()

    def fire_listener_callbacks(self):
        for listener in self._listeners:
            listener.fire_callbacks()


class InputCell(Cell):
    @Cell.value.setter
    def value(self, val):
        self._value = val
        self.recompute_listeners()
        self.fire_listener_callbacks()


class ComputeCell(Cell):
    def __init__(self, inputs, compute_function):
        super().__init__()
        self.inputs = inputs
        for cell in inputs:
            cell.add_listener(self)
        self.formula = compute_function
        self.callbacks = []
        self.compute()
        self.previous = self.value

    def compute(self):
        values = [cell.value for cell in self.inputs]
        self.value = self.formula(values)

    def recompute(self):
        self.compute()
        self.recompute_listeners()

    def fire_callbacks(self):
        val = self.value
        if self.previous != val:
            self.previous = val
            for cb in self.callbacks:
                cb(val)
            self.fire_listener_callbacks()

    def add_callback(self, callback):
        ''' TODO
        should we allow the same callback multiple times?
        '''
        self.callbacks.append(callback)

    def remove_callback(self, callback):
        ''' TODO
        "Guard against incorrect implementations which store
        their callbacks in an array."
        What should the implementation be?
        '''
        while callback in self.callbacks:
            self.callbacks.remove(callback)
