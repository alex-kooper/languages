
class Point(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def move(self, dx, dy):
        return Point(self.x + dx, self.y + dy)

    def rotate_right(self, p):
        new_x = -(self.y - p.y) + p.x
        new_y = (self.x - p.x) + p.y
        return Point(new_x, new_y)

    def rotate_left(self, p):
        new_x = (self.y - p.y) + p.x
        new_y = -(self.x - p.x) + p.y
        return Point(new_x, new_y)

    def reflect_vertically(self, x):
        return Point(2 * x - self.x, self.y)

    def reflect_horizontally(self, y):
        return Point(self.x, 2 * y - self.y)

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, p):
        return (self.x, self.y) == (p.x, p.y)

    def __cmp__(self, p):
        return cmp((self.x, self.y), (p.x, p.y))

    def __repr__(self):
        return "Point(" + str(self.x) + ", " + str(self.y) + ")"

