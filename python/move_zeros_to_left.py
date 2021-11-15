# Given an integer array, move all elements that are 0 to the left while
# maintaining the order of other elements in the array. The array has to be
# modified in-place. 

def move_zeros_to_left(a):
    read_i = write_i = len(a) - 1
    
    while read_i >= 0:
        if a[read_i] == 0:
            read_i -= 1
        else:
            a[write_i] = a[read_i]
            read_i -= 1
            write_i -= 1

    for i in range(0, write_i + 1):
        a[i] = 0

a = [1, 10, 20, 0, 59, 63, 0, 88, 0]

print("Original Array:", a)

move_zeros_to_left(a)

print("After Moving Zeroes to Left: ", a)
