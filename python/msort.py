def msort(l):
    if len(l) <= 1:
        return l

    middle = len(l) / 2
    return merge(msort(l[:middle]), msort(l[middle:]))
    
def merge(l1, l2):
    if len(l1) == 0:
        return l2

    if len(l2) == 0:
        return l1

    if l1[0] < l2[0]:
        return [l1[0]] + merge(l1[1:], l2)
    else:
        return [l2[0]] + merge(l1, l2[1:])


