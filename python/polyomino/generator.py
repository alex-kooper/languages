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

