from itertools import product
from functools import partial

class Board(object):

    def __init__(self, height, width):
        self.height = height
        self.width = width

        self._board = [[None] * width for i in range(height)]
        self.moves = []

    @property
    def last_position(self):
        return self.moves[-1]

    def move_to(self, position):
        self.moves.append(position)
        x, y = position
        self._board[self.height - y - 1][x] = len(self.moves)

    def undo(self):
        x, y = self.moves.pop()
        self._board[self.height - y - 1][x] = None 

    def visited(self, position):
        x, y = position
        return self._board[self.height - y - 1][x]

    def all_visited(self):
        return self.width * self.height <= len(self.moves)

    def __str__(self):
        def row_to_str(row):
            return ''.join('{:2d} '.format(c) for c in row)

        return '\n'.join(row_to_str(r) for r in self._board)

    __repr__ = __str__


moves_deltas = [(x, y) 
    for x, y in product(range(-2, 3), range(-2, 3))
    if abs(x) != abs(y) and x != 0 and y != 0
]

def moves_from_position(board, position=None):
    x, y = position or board.last_position
    moves = ((x + dx, y + dy) for dx, dy in moves_deltas)

    return (
        (x, y)
        for x, y in moves 
        if x in range(board.width) and 
           y in range(board.height) and
           not board.visited((x, y))
    )
    
def n_moves_from_position(board, position):
    return len(list(moves_from_position(board, position)))

def has_solution(board):
    if board.all_visited():
        return True

    moves = sorted(moves_from_position(board), 
                   key=partial(n_moves_from_position, board)) 

    for p in moves:
        board.move_to(p)

        if has_solution(board):
            return True

        board.undo()

    return False    

def solve(height, width):
    board = Board(height, width)

    for x in range(width):
        for y in range(height):
            board.move_to((x, y))

            if has_solution(board):
                return board

            board.undo()

    return None


print(solve(5, 50))

