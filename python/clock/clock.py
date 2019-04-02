class Clock(object):
    MINS_PER_DAY = 24 * 60

    def __init__(self, hour, minute):
        self.minute = (60 * hour + minute) % self.MINS_PER_DAY

    def __repr__(self):
        return "%02d:%02d" % divmod(self.minute, 60)

    def __eq__(self, other):
        return self.minute == other.minute

    def __add__(self, minutes):
        self.minute = (self.minute + minutes) % self.MINS_PER_DAY
        return self

    def __sub__(self, minutes):
        return self + (-minutes)
