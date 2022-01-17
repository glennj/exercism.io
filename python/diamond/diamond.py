def rows(letter):
    A = ord('A')
    size = ord(letter) - A + 1
    lines = []
    # build the bottom half
    for i in range(size):
        half = (" " * i) + chr(A + i) + (" " * (size-i-1))
        lines.insert(0, half[-1:0:-1] + half)
    # reverse and concat
    return lines[-1:0:-1] + lines
