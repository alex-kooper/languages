
class Node:
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def __repr__(self):
        return "Node(%s, %s, %s)" % (
            repr(self.value), repr(self.left), repr(self.right)
        )

def traverse(node):
    yield node

    if node.left:
        for n in traverse(node.left):
            yield n

    if node.right:
        for n in traverse(node.right):
            yield n

tree = Node('Root', 
           Node('Node1'), 
           Node('Node2',
               Node('Node3'),
               Node('Node4')
           )
       )

print [n.value for n in traverse(tree)]

