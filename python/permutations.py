from itertools import chain


def permutations(xs):
    if not xs:
        yield []
        return

    for x in xs:
        new_xs = [x1 for x1 in xs if x1 != x]
        for p in permutations(new_xs):
            yield [x] + p
