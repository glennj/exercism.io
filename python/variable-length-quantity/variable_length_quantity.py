SHIFT_AMT = 7
MSB  = 0b10000000
MASK = 0b01111111


def encode(numbers):
    bytes_ = []
    for num in reversed(numbers):
        msb = 0
        while True:
            byte = (num & MASK) | msb
            bytes_.insert(0, byte)
            msb = MSB
            num >>= SHIFT_AMT
            if num == 0:
                break
    return bytes_


def decode(bytes_):
    if bytes_[-1] & MSB != 0:
        raise ValueError('incomplete sequence')
    numbers = []
    num = 0
    for byte in bytes_:
        num = (num << SHIFT_AMT) + (byte & MASK)
        if byte & MSB == 0:
            numbers.append(num)
            num = 0
    return numbers
