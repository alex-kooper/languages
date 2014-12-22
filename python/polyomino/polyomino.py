from point import Point

class Polyomino(object):

    def __init__(self, *args):
        if len(args) > 1 or isinstance(args[0], tuple):
            self.points = frozenset(Point(x, y) for (x, y) in args)
        else:
            self.points = frozenset(args[0])

    def add(self, point):
        return Polyomino(self.points | {point})

    def __contains__(self, point):
        return point in self.points

    def upper_left_corner(self):
        x = min(p.x for p in self.points)
        y = min(p.y for p in self.points)
        return Point(x, y)

    def lower_right_corner(self):
        x = max(p.x for p in self.points)
        y = max(p.y for p in self.points)
        return Point(x, y)

    def width(self):
        return self.lower_right_corner().x - self.upper_left_corner().x + 1

    def height(self):
        return self.lower_right_corner().y - self.upper_left_corner().y + 1

    def move(self, dx, dy):
        return Polyomino(p.move(dx, dy) for p in self.points)

    def rotate_right(self, point=Point.origin):
        return Polyomino(p.rotate_right(point) for p in self.points) 

    def rotate_left(self, point=Point.origin):
        return Polyomino(p.rotate_left(point) for p in self.points) 
    
    def reflect_vertically(self, x=0):
        return Polyomino(p.reflect_vertically(x) for p in self.points)

    def reflect_horizontally(self, y=0):
        return Polyomino(p.reflect_horizontally(y) for p in self.points)

    def move_to_origin(self):
        ulc = self.upper_left_corner()
        return self.move(-ulc.x, -ulc.y) 

    def render(self):
        polyomino = self.move_to_origin()

        def render_cell(x, y): 
            return "[]" if Point(x, y) in polyomino.points else "  "

        s = "\n".join(
            "".join(render_cell(x, y) for x in range(self.width()))
            for y in range(self.height())
        )

        return "\n" + s + "\n" 

    def all_rotations(self):
        polyomino = self.move_to_origin()
        rotations = {polyomino}

        for i in xrange(3):
            polyomino = polyomino.rotate_right().move_to_origin() 
	    rotations.add(polyomino)

        return rotations

    def all_congruents(self):
        return self.all_rotations() | self.reflect_horizontally().all_rotations()

    def normalize(self):
        return max(p for p in self.all_congruents() if p.width() >= p.height())

    def __hash__(self):
        return hash(self.points)

    def __eq__(self, other):
        return self.points == other.points 

    def __cmp__(self, other):
        return cmp(sorted(self.points), sorted(other.points))

    def __repr__(self):
        return "Polyomino(" + str(list(self.points)) + ")" 

    def __str__(self):
        return self.render()

