# stealing from this brilliant solution:
# https://exercism.io/tracks/python/exercises/rail-fence-cipher/solutions/8d7425bdbb844c5e9416015cd7eb3daa

from itertools import cycle


def rail_pattern(n):     # given n == 5
    r = list(range(n))   # then: 0,1,2,3,4
    r += r[-2:0:-1]      # and:  + 3,2,1
    return cycle(r)


# @tcarobruce, brilliant
def railed_indices(text, n):
    rp = rail_pattern(n)
    return sorted(range(len(text)), key=lambda _: next(rp))


def encode(plaintext, rails):
    indices = railed_indices(plaintext, rails)
    encoded = [plaintext[i] for i in indices]
    return ''.join(encoded)


def decode(ciphertext, rails):
    indices = railed_indices(ciphertext, rails)
    # Schwartzian transform:
    # I'm relying on `sorted` being a stable sort.
    pairs = zip(indices, ciphertext)
    pairs = sorted(pairs, key=lambda pair: pair[0])
    decoded = map(lambda pair: pair[1], pairs)
    return ''.join(decoded)


"""
# take 1
def encode(message, num_rails):
    if num_rails == 1 or num_rails > len(message):
        return message
    p = RailsPointer(num_rails)
    rails = [''] * num_rails
    for ch in message:
        rails[p.rail] += ch
        p.incr()
    return ''.join(rails)


def decode(encoded_message, num_rails):
    if num_rails == 1 or num_rails > len(encoded_message):
        return encoded_message
    rails = partition(encoded_message, num_rails)
    plaintext = ''
    p = RailsPointer(num_rails)
    for i in range(len(encoded_message)):
        plaintext += rails[p.rail][:1]
        rails[p.rail] = rails[p.rail][1:]
        p.incr()
    return plaintext


def partition(encoded, num_rails):
    rails = []
    for length in partition_lengths(encoded, num_rails):
        rails.append(encoded[:length])
        encoded = encoded[length:]
    return rails


def partition_lengths(encoded, num_rails):
    '''
     i. each journey down and up the rails consumes
        2 * (num_rails - 1) characters
    ii. the "inner" rails consume twice as many as
        the top & bottom rails
    '''
    base_len, rem = divmod(len(encoded), 2 * (num_rails - 1))
    lengths = [
        base_len * (2 if 1 <= i < num_rails - 1 else 1)
        for i in range(num_rails)
    ]
    # handle the leftovers, 1 per rail starting from the top
    p = RailsPointer(num_rails)
    for i in range(rem):
        lengths[p.rail] += 1
        p.incr()
    return lengths


class RailsPointer:
    def __init__(self, num_rails):
        self.rail = 0
        self.dir = +1
        self.nr = num_rails

    def incr(self):
        if (self.dir == +1 and self.rail == self.nr - 1) \
        or (self.dir == -1 and self.rail == 0):
            self.dir *= -1
        self.rail += self.dir
"""
