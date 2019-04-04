def square_of_sum(count):
    return (count * (count+1)/2)**2
    # return sum(range(count + 1))**2


def sum_of_squares(count):
    return (count * (count+1) * (2*count+1)) / 6
    # return sum(x**2 for x in range(count + 1))


def difference(count):
    return square_of_sum(count) - sum_of_squares(count)
