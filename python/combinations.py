
def combinations(xs, n):
    if n < 0:
        raise Exception(f"Invalid length: {n}")
    
    if len(xs) < n:
        return 

    if n == 0:
        yield []
        return

    for x in combinations(xs[1:], n - 1):
        yield [xs[0]] + x

    for x in combinations(xs[1:], n):
        yield x

