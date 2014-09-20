from itertools import cycle, count, takewhile, izip
from fractions import Fraction

def pi(p):
    denoms = takewhile(lambda n: n < 4 * pow(10, p), count(1, 2))
    signs = cycle([1, -1])
    series = (4. / (d * s) for (d, s) in izip(denoms, signs))
    return round(sum(series), p)

