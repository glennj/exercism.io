from functools import partialmethod


class SpaceAge(object):

    sec_per_yr = 31557600

    periods = {
        'mercury':   0.2408467,
        'venus':     0.61519726,
        'earth':     1.0,
        'mars':      1.8808158,
        'jupiter':  11.862615,
        'saturn':   29.447498,
        'uranus':   84.016846,
        'neptune': 164.79132,
    }

    def __init__(self, seconds):
        self.seconds = seconds
        for planet, period in self.periods.items():
            age = round(seconds / self.sec_per_yr / period, 2)
            setattr(self, f'on_{planet}', lambda a=age: a)

    ''' take 1
    def _calc(self, period):
        return round(self.seconds / self.sec_per_yr / period, 2)

    for planet, period in periods.items():
        eval(compile(
            f"on_{planet} = partialmethod(_calc, {period})",
            '<string>',
            'exec'
        ))
    '''
