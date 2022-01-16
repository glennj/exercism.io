import string


class PhoneNumber(object):
    def __init__(self, phone_number):
        parts = self.parse(phone_number)
        self.number = "".join(parts)
        self.area_code = parts[0]
        self.formatted = f"({parts[0]})-{parts[1]}-{parts[2]}"

    def pretty(self):
        return self.formatted

    def parse(self, phone_number):
        # remove legal non-digits
        chars = [c for c in phone_number.lstrip('+')
                    if c not in f'().-{string.whitespace}']

        if any(c in string.ascii_letters for c in chars):
            raise ValueError("letters not permitted")
        if any(c in string.punctuation for c in chars):
            raise ValueError("punctuations not permitted")

        digits = [d for d in chars if d in string.digits]
        if len(digits) < 10:
            raise ValueError("incorrect number of digits")
        if len(digits) > 11:
            raise ValueError("more than 11 digits")
        if len(digits) == 11:
            if digits[0] == '1':
                digits = digits[1:]
            else:
                raise ValueError("11 digits must start with 1")

        if digits[0] == '0':
            raise ValueError("area code cannot start with zero")
        if digits[0] == '1':
            raise ValueError("area code cannot start with one")
        if digits[3] == '0':
            raise ValueError("exchange code cannot start with zero")
        if digits[3] == '1':
            raise ValueError("exchange code cannot start with one")

        return [''.join(part) for part in [digits[0:3], digits[3:6], digits[6:]]]
