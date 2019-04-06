import re


class Phone(object):
    def __init__(self, phone_number):
        p = re.sub(r'\D', '', phone_number)
        m = re.match(r'^1?(([2-9]\d\d)([2-9]\d\d)(\d{4}))$', p)
        if not m: raise ValueError('Invalid phone number')
        parts = m.groups()
        self.number = parts[0]
        self.area_code = parts[1]
        self.formatted = "({}) {}-{}".format(*parts[1:])

    def pretty(self):
        return self.formatted
