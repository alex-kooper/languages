
def qsort(lst):
    if not lst:
        return lst

    pivot, others = lst[0], lst[1:]
    smaller = qsort([e for e in others if e < pivot])
    bigger = qsort([e for e in others if e >= pivot])

    return smaller + [pivot] + bigger

