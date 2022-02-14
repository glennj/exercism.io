def can_chain(dominoes):
    if not dominoes:
        return []

    for i, d in enumerate(dominoes):
        rest = dominoes[:i] + dominoes[i+1:]
        chain = chain_from([d], rest)
        if not chain:
            # try again, but reverse the starting domino
            chain = chain_from([d[::-1]], rest)
        if chain:
            return chain

    # no chain possible with these dominoes
    return None


def chain_from(chain, remaining):
    tail = chain[-1][1]
    if not remaining:
        return chain if chain[0][0] == tail else None

    for i, d in enumerate(remaining):
        if tail in d:
            if d[0] != tail:
                d = d[::-1]
            rest = remaining[:i] + remaining[i+1:]
            result = chain_from(chain + [d], rest)
            if result:
                return result
    return None
