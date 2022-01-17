def square_of_sum(count):
    return (count * (count+1)/2)**2
    # return sum(range(count + 1))**2


def sum_of_squares(count):
    return (count * (count+1) * (2*count+1)) / 6
    # return sum(x**2 for x in range(count + 1))


def difference_of_squares(count):
    return abs(sum_of_squares(count) - square_of_sum(count))
