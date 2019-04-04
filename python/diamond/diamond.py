def make_diamond(letter):
    A = ord('A')
    size = ord(letter) - A + 1
    lines = []
    # built the bottom half
    for i in range(size):
        half = (" " * i) + chr(A + i) + (" " * (size-i-1))
        lines.insert(0, half[-1:0:-1] + half + "\n")
    # reverse and concat
    return "".join(lines[-1:0:-1] + lines)
