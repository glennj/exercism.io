class BowlingGame(object):
    def __init__(self):
        self._score = 0
        self._frame = 1
        self._current = []      # rolls in the current frame
        self._bonuses = []      # strikes/spares wer're counting

    def roll(self, pins):
        if self._frame > 10:
            raise IndexError("no rolls after game over")
        if pins < 0 or pins > 10:
            raise ValueError("invalid roll")
        if self.too_many(pins):
            raise ValueError("too many pins for frame")

        self.add_score(pins)
        self.handle_frame(pins)

    def score(self):
        if self._frame <= 10:
            raise IndexError("game not over yet")
        return self._score

    def too_many(self, pins):
        if len(self._current) == 0:
            return False
        if self._frame == 10:
            return self.too_many_tenth(pins)
        return self._current[0] + pins > 10

    def too_many_tenth(self, pins):
        non_strikes = list(filter(
            lambda roll: roll < 10,
            self._current
        ))
        num = len(non_strikes)
        if num == 0 or (num == 2 and sum(non_strikes) == 10):
            return False
        return non_strikes[0] + pins > 10

    def add_score(self, pins):
        self._score += pins
        for i, b in enumerate(self._bonuses):
            self._score += pins
            self._bonuses[i] -= 1
        self._bonuses = list(filter(
            lambda b: b > 0,
            self._bonuses
        ))

    def handle_frame(self, pins):
        if self._frame == 10:
            self.handle_tenth_frame(pins)
        else:
            self.handle_nth_frame(pins)

    def handle_nth_frame(self, pins):
        if pins == 10:
            # strike
            self._bonuses.append(2)
            self._frame += 1
        elif len(self._current) == 0:
            self._current.append(pins)
        elif len(self._current) == 1:
            if self._current[0] + pins == 10:
                # spare
                self._bonuses.append(1)
            self._frame += 1
            self._current = []

    def handle_tenth_frame(self, pins):
        self._current.append(pins)
        num = len(self._current)
        if num == 3 or (num == 2 and sum(self._current) < 10):
            self._frame += 1
