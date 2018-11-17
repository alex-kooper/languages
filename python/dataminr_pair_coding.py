"""
Parse a string that has phone ids and their quantities.
IDs can be either for Android or IPhone.
Android format is A + 4 characters.
IPhone format is I + 3 characters. 
"""
import re
from itertools import takewhile, dropwhile, izip

EXAMPLE = "Aa1133Iab2Aa112Iac3"

def tokens(s):
    pattern = r'A\w{3}|I\w{2}|\d+' 
    return (g.group() for g in re.finditer(pattern, s))

def tokens_to_pairs(tokens):
    i = iter(tokens)
    return ((ident, int(quant)) for (ident, quant) in izip(i, i))

def parse_into_map(s):
    d = {}

    for (k, v) in tokens_to_pairs(tokens(s)):
        d.update({k: d.get(k, 0) + v})

    return d

print parse_into_map(EXAMPLE)

