def combinations(target, size, exclude):
    if size == 1:
        if 1 <= target <= 9 and target not in exclude:
            return [[target]]
        else:
            return []
    else:
        results = []
        for n in range(1, 10):
            if n not in exclude:
                for c in combinations(target - n, size - 1, exclude + [n]):
                    combination = sorted([n, *c])
                    if combination not in results:
                        results.append(combination)
        return results

