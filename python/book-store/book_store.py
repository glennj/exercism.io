from collections import defaultdict
from functools import reduce


reductions = (1, 0, 0.05, 0.10, 0.20, 0.25)
book_price = 800


def total(books):
    return min(map(price, get_discounts(books)))


def price(discount):
    return round(book_price * sum(
        i * (1 - reductions[i]) for i in discount
    ))


def get_discounts(books):
    discounts = []
    # this is the "no discount" discount: each book full price
    discounts += [[1] * len(books)]
    discounts += find_discounts(books)
    # for each set of discounts, pad to the number of books
    return [
        d + [1] * (len(books) - sum(d))
        for d in discounts
    ]


def find_discounts(books):
    if len(books) < 2:
        return []
    discounts = []
    for i in range(5, 0, -1):
        if contains_set(books, i):
            discounts.append([i])
            remaining = remove_set(books, i)
            for d in find_discounts(remaining):
                discounts += [[i] + d]
    return discounts


def contains_set(books, n):
    ''' does this basket contain a set of `n` books '''
    return len(set(books)) >= n


def remove_set(books, n):
    counts = defaultdict(int)
    for b in books:
        counts[b] += 1

    basket = sorted(
        counts.keys(),
        key=lambda b: -counts[b]
    )

    result = books[:]
    for b in basket[:n]:
        result.remove(b)
    return result
