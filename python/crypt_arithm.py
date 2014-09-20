from itertools import permutations

def letter_maps(letters):
    # Generate all possible mappings from letters to digits 
    perms =  permutations('0123456789', len(letters))
    return (dict(zip(letters, digits)) for digits in perms)

def replace_letters(crypt, letter_map):
    return ''.join(letter_map.get(c, c) for c in crypt)

def crypt_letters(crypt):
    return set(c for c in crypt if c.isalpha())

def candidates(crypt):
    maps = letter_maps(crypt_letters(crypt))
    return (replace_letters(crypt, m) for m in maps)

def is_solution(candidate):
    try:
        return eval(candidate.replace('=', '=='))
    except:
        return False
    
def solve(crypt):
    """ Return all possible solutions to the crypt in iterator

    solve is a generator that lazily generates all possible solutions 
    to the given crypt
    """
    return (c for c in candidates(crypt) if is_solution(c))

def main():
    crypt = "SEND + MORE = MONEY"
    print "Solving: ", crypt
    print "Letters: ", ', '.join(sorted(crypt_letters(crypt)))

    print "\nPossible solutions"
    for s in solve(crypt):
        print s

if __name__ == "__main__":
    main()

