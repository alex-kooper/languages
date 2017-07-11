
class Stream(object):
    def __rpow__(self, other):
        return Cons(other, self)

class NilStream(Stream):
    def __repr__(self):
        return 'Nil'

    def take(self, n):
        return self

    def map(self, f):
        return self

Nil = NilStream()

class Cons(Stream):
    def __init__(self, head, tail):
        self.head = head
        self._tail = tail

    @property
    def tail(self):
        return self._tail() if isinstance(self._tail, Lazy) else self._tail

    def __repr__(self):
        return repr(self.head) + ' ** ' + repr(self._tail)

    def take(self, n):
        return self.head ** self.tail.take(n - 1) if n > 0 else Nil

    def map(self, f):
        return f(self.head) ** lazy(lambda: self.tail.map(f)) 

class Lazy(Stream):
    def __init__(self, fn):
        self.fn = fn

    def __call__(self):
        return self.fn()

    def __repr__(self):
        return '...'

lazy = Lazy

ones = 1 ** lazy(lambda: ones)
nums = 1 ** lazy(lambda: nums.map(lambda x: x + 1))

