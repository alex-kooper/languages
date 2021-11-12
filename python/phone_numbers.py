from itertools import product

DIGITS_TO_CHARS = {
    '0': '0',
    '1': '1',
    '2': 'abc',
    '3': 'def',
    '4': 'ghi',
    '5': 'jkl',
    '6': 'mno',
    '7': 'pqrs',
    '8': 'tuv',
    '9': 'wxyz'
}

def phone_number_to_words(s):
    tuples = product(*(DIGITS_TO_CHARS[c] for c in s))
    result = sorted(''.join(list(t)) for t in tuples)
    return ','.join(result) 


print(phone_number_to_words('2453270'))

