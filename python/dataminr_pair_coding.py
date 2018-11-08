"""
Parse a string that has phone ids and their quantities.
IDs can be either for Android or IPhone.
Android format is A + 4 characters.
IPhone format is I + 3 characters. 
"""
from itertools import takewhile, dropwhile, izip

EXAMPLE = "Aa1133Iab2Aa112Iac3"

def tokens(input_string):
    rest = input_string

    while rest:
        if rest.startswith('A'):
            yield rest[0:4]
            rest = rest[4:]
        elif rest.startswith('I'):
            yield rest[0:3]
            rest = rest[3:]
        elif rest[0].isdigit:
            i = takewhile(lambda c: c.isdigit(), rest)
            yield int(''.join(i))
            rest = ''.join(dropwhile(lambda c: c.isdigit(), rest))
        else:
            raise Exception("Invalid String!")

def tokens_to_pairs(tokens):
    i = iter(tokens)
    return izip(i, i)

def parse_into_map(s):
    d = {}

    for (k, v) in tokens_to_pairs(tokens(s)):
        d.update({k: d.get(k, 0) + v})

    return d

print parse_into_map(EXAMPLE)

