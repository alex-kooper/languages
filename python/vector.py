
class Vector(object):
    def __init__(self, size=None, value=None, data=None):
        if data is not None:
            self._data = list(data)
            return

        self._data = [value] * size 

    def __add__(self, other):
        data = [a + b for (a, b) in zip(self._data, other._data)]
        return Vector(data=data)

    def __mul__(self, other):
        try:
            return sum(a * b for (a, b) in zip(self._data, other._data))
        except:
            return Vector(data=[a * other for a in self._data])

    __rmul__ = __mul__

    def __pow__(self, power):
        return sum(a ** power for a in self._data)

    def __sub__(self, other):
        return self + (other * -1)

    def __repr__(self):
        return str(self._data)
    
    def __getitem__(self, i):
        return self._data[i]
    
    def __setitem__(self, i, value):
        self._data[i] = value

    def __len__(self):
        return len(self._data)

