from itertools import permutations

def get_value(char, mapping):
    return int(char) if char.isdigit() else mapping.get(char) 

def add_digits(a, b, carry=0):
    c = a + b + carry
    return (c % 10, c / 10) if c > 9 else (c, 0)
    
def one_column_mappings(arg1, arg2, result, mapping={}, carry=0):
    letters = arg1 + arg2
    letters = {c for c in letters if c.isalpha() and not mapping.has_key(c)}

    perms = permutations(set(range(10)) - set(mapping.values()), len(letters))
    mappings = (dict(zip(letters, digits)) for digits in perms)

    for m in mappings:
        new_mapping = dict(mapping)
        new_mapping.update(m)

        expected_result, new_carry = add_digits(
            get_value(arg1, new_mapping), 
            get_value(arg2, new_mapping),
            carry
        )

        assigned_result = get_value(result, new_mapping)

        if assigned_result == None:
            new_mapping[result] = expected_result 
            yield (new_mapping, new_carry) 
            continue

        if assigned_result == expected_result:
            yield (new_mapping, new_carry)

def mappings(arg1, arg2, result, mapping={}, carry=0):
    if not result:
        if carry == 0:
            yield mapping
        return

    arg1_letter = arg1[-1] if arg1 else '0'
    arg2_letter = arg2[-1] if arg2 else '0'
    result_letter = result[-1] if result else '0'
    
    arg1 = arg1[:-1] if arg1 else arg1
    arg2 = arg2[:-1] if arg2 else arg2

    result = result[:-1]

    for m1, c in one_column_mappings(arg1_letter, 
                                     arg2_letter, 
                                     result_letter, 
                                     mapping, 
                                     carry):

        for m2 in mappings(arg1, arg2, result, m1, c):
            yield m2

def subs_mapping(crypt, mapping):
    return ''.join(str(mapping.get(c, c)) for c in crypt)

def solve(crypt):
    args, result = crypt.replace(" ", "").split('=')
    arg1, arg2 = args.split('+')

    for m in mappings(arg1, arg2, result):
        if m[arg1[0]] != 0 and m[arg2[0]] != 0 and m[result[0]] != 0:
            yield subs_mapping(crypt, m) 

    
