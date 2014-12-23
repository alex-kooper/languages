from __future__ import print_function

from polyomino import Polyomino

def generate(n_points):
    if n_points == 1:
        return { Polyomino((0, 0)) }

    return { p2 for p1 in generate(n_points - 1) 
                for p2 in generate_by_adding_one_point(p1) }
    
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
    
    polyominoes = generate(n)
    
    print('There are {0} polyominoes with  {1} cells'
        .format(len(polyominoes), n))

    print('Do you want me to show all of them? [y/n]: ', end='')
    yesOrNo = raw_input()

    if yesOrNo.lower() == 'y':
        for p in polyominoes:
            print(p.render())

if __name__ == "__main__":
    main()    

