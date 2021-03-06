from __future__ import print_function
from itertools import chain
from datetime import datetime

from polyomino import Polyomino

def generate(n_points):
    if n_points == 1:
        return { Polyomino((0, 0)) }
    
    polyominoes = (generate_by_adding_one_point(p) 
                   for p in generate(n_points - 1))

    return set(chain.from_iterable(polyominoes))
    
def generate_by_adding_one_point(polyomino):
    adjacent_point_deltas = ((-1, 0), (0, -1), (1, 0), (0, 1))
    polyominoes = set() 

    for p in polyomino.points:
        for (dx, dy) in adjacent_point_deltas:
            new_point = p.move(dx, dy)
            if new_point not in polyomino:
                polyominoes.add(polyomino.add(new_point).normalize())

    return polyominoes

def main():
    print('Enter number of cells: ', end='')
    n = int(raw_input())
    
    start = datetime.now()
    polyominoes = generate(n)
    end = datetime.now()

    print('There are {0} polyominoes with {1} cells'
        .format(len(polyominoes), n))

    print('It took {0} to generate them'.format(end - start))

    print('Would you like to see all of them? [y/n]: ', end='')
    yesOrNo = raw_input()

    if yesOrNo.lower() == 'y':
        for p in polyominoes:
            print(p.render())

if __name__ == "__main__":
    main()    

