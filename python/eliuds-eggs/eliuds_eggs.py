def egg_count(num):
    count = 0

    while num > 0:
        count += num & 1
        num >>= 1

    return count
