'''
We need python to be able to:
1. construct an array: `[a, b, c]`
2. spread and slurp arrays: `a, b, *rest = [*xs]`
3. tell us if an array is empty: `if xs: print('not empty')`
'''


def push(xs, x):
    return [*xs, x]


def unshift(xs, x):
    return [x, *xs]


def foldl(f, xs, acc):
    while xs:
        x, *xs = xs
        acc = f(acc, x)
    return acc


''' We have all we need to do the rest: '''


def length(xs):
    return foldl(lambda acc, _: acc + 1, xs, 0)


def append(xs, ys):
    return foldl(push, ys, xs)


def concat(lists):
    return foldl(append, lists, [])


def reverse(xs):
    return foldl(unshift, xs, [])


def map_clone(f, xs):
    return foldl(lambda acc, x: push(acc, f(x)), xs, [])


def filter_clone(f, xs):
    return foldl(
        lambda acc, x: push(acc, x) if f(x) else acc,
        xs,
        []
    )


def foldr(f, xs, acc):
    return foldl(lambda a, x: f(x, a), reverse(xs), acc)
