from itertools import islice

def List(*args):
    if not args:
        return None

    if len(args) ==  1:
        return ListNode(args[0])

    return ListNode(args[0], List(*args[1:]))
    
class ListNode(object):
    def __init__(self, head, tail=None):
        self.head = head
        self.tail = tail

    def __iter__(self):
        return ListIter(self) 

    def __str__(self):
        return str(list(self))

    __repr__ = __str__

    def concatenate(self, list):
        if not self.tail:
            return ListNode(self.head, list)

        return ListNode(self.head, self.tail.concatenate(list))

    __add__ = concatenate

    def __getitem__(self, slice):
        if isinstance(slice, int):
            return List(*islice(self, slice, slice + 1))

        return List(*islice(self, slice.start, slice.stop, slice.step))

class ListIter(object):
    def __init__(self, list):
        self.__list = list

    def next(self):
        if not self.__list:
            raise StopIteration()

        elem = self.__list.head
        self.__list = self.__list.tail
        return elem

def head(list):
    return list.head

def tail(list):
    return list.tail

def add(elem, list):
    return ListNode(elem, list)


