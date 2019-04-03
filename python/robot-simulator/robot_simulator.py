from math import radians, cos, sin
from functools import partialmethod

EAST = 0
NORTH = 90
WEST = 180
SOUTH = 270


class Robot(object):
    def __init__(self, bearing=NORTH, x=0, y=0):
        self.bearing = bearing
        self.coordinates = (x, y)

    def turn(self, dir):
        self.bearing = (self.bearing + 90 * dir) % 360

    turn_right = partialmethod(turn, dir=-1)
    turn_left  = partialmethod(turn, dir=+1)

    def advance(self):
        rad = radians(self.bearing)
        x, y = self.coordinates
        dx, dy = int(cos(rad)), int(sin(rad))
        self.coordinates = (x + dx, y + dy)

    def simulate(self, script):
        dispatch = {
            'R': self.turn_right,
            'L': self.turn_left,
            'A': self.advance
        }
        for cmd in script:
            if cmd in dispatch:
                dispatch[cmd]()
            else:
                raise ValueError(f'Unknown instruction {cmd}')
