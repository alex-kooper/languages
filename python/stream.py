from operator import add

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

    def zip_with(self, f, other):
        return self

Nil = NilStream()

class Cons(Stream):
    def __init__(self, head, tail):
        self.head = head
        self._tail = tail

    @property
    def tail(self):
        if isinstance(self._tail, LazyFunction):
            self._tail = self._tail()

        return self._tail 

    def __repr__(self):
        return repr(self.head) + ' ** ' + repr(self._tail)

    def take(self, n):
        if n > 0:
            return self.head ** lazy_fn(lambda: self.tail.take(n - 1))
        elif n == 1:
            return self.head ** Nil
        else:
            return Nil

    def map(self, f):
        return f(self.head) ** lazy_fn(lambda: self.tail.map(f)) 

    def zip_with(self, f, other):
        if other is Nil:
            return Nil

        return f(self.head, other.head) ** lazy_fn(lambda: self.tail.zip_with(f, other.tail))

    def __getitem__(self, n):
        node = self

        try:
            for i in xrange(n): 
                node = node.tail
        except:
            raise IndexError('list index out of range')

        return node.head


    def to_list(self):
        result = []
        current = self

        while current is not Nil:
            result.append(current.head)
            current = current.tail

        return result


class LazyFunction(Stream):
    def __init__(self, fn):
        self.function = fn

    def __call__(self):
        return self.function()

    def __repr__(self):
        return '...'

lazy_fn = LazyFunction

# Examples of using the lazy stream

ones = 1 ** lazy_fn(lambda: ones)
nums = 1 ** lazy_fn(lambda: nums.map(lambda x: x + 1))

def start_from(n):
    return n ** lazy_fn(lambda: start_from(n + 1))

fibs = 0 ** 1 ** lazy_fn(lambda: fibs.zip_with(add, fibs.tail))

