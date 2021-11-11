
# Facebook Interview Sample
#
# 2D Spiral Array
#
# Find the pattern and complete the function:
# int[][] spiral(int n);
# where n is the size of the 2D array.
#
# Sample Result
#
# input = 3
# 123
# 894
# 765
#
# input = 4
# 01 02 03 04
# 12 13 14 05
# 11 16 15 06
# 10 09 08 07

def rotate_right(dx, dy):
    return (-dy, dx)


def make_2d_spiral(n):
    result = [[None for y in range(n)] for x in range(n)]

    def is_valid(x, y):
        return x >= 0 and x < n and y >= 0 and y < n and (result[y][x] is None)

    x = 0
    y = 0

    dx = 1
    dy = 0

    current_number = 1

    while True:
        result[y][x] = current_number
        current_number += 1

        if is_valid(x + dx, y + dy):
            x += dx
            y += dy
        else:
            (dx, dy) = rotate_right(dx, dy)

            if is_valid(x + dx, y + dy):
                x += dx
                y += dy

            else:
                return result


def print_2d_spiral(matrix):
    n = len(matrix)
    fmt = '{{:0{:d}d}}'.format(len(str(n * n)))

    result = ('\n'.join(
        ' '.join(
            fmt.format(matrix[y][x])
            for x in range(n)
        )
        for y in range(n)
    ))

    print(result)


def make_and_print_2d_spiral(n):
    print_2d_spiral(make_2d_spiral(n))
