# Facebook interview sample "Look and Say"

# Implement a function that outputs the Look and Say sequence:
#
# 1
# 11
# 21
# 1211
# 111221
# 312211
# 13112221
# 1113213211
# 31131211131221
# 13211311123113112211

from itertools import groupby, chain, islice


def next(n):
    return ''.join(
        chain.from_iterable(
            str(len(list(g))) + k for (k, g) in groupby(n)
        )
    )


def look_and_say():
    current = '1'

    while True:
        yield current
        current = next(current)


def print_look_and_say(n):
    for s in islice(look_and_say(), n):
        print(s)
