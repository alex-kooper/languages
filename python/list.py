
def List(*args):
    if args:
        return (args[0], List(*args[1:]))
    else:
        return None

def head(list):
    return list[0]

def tail(list):
    return list[1]

def length(list):
    return 1 + length(tail(list)) if list else 0

def concat(l1, l2):
    if not l1:
        return l2

    return (head(l1), concat(tail(l1), l2))

def map(f, list):
    if list:
        return (f(head(list)), map(f, tail(list)))

    return None

def filter(p, list):
    if not list:
        return None

    if p(head(list)):
        return (head(list), filter(p, tail(list)))
    else:
        return filter(p, tail(list))

