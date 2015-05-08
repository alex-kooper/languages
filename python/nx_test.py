'''
import networkx
from qz.lib.timeit import Timer

nodeNumber = 0

def generateNode(graph):
    global nodeNumber
    
    nodeNumber += 1
    
    label = 'Node ' * 10  + str(nodeNumber)
    graph.add_node(label)
    
    graph.node[label]['attribute1'] = 'Attribute1 ' + str(nodeNumber) 
    graph.node[label]['attribute2'] = 'Attribute2 ' + str(nodeNumber) 
    graph.node[label]['attribute3'] = 'Attribute3 ' + str(nodeNumber) 
    
    return label

def generateTree(graph, nLevels, nChildren):
    global nodeNumber
    
    if nLevels == 1:
        return generateNode(graph)
    
    children = (generateTree(graph, nLevels - 1, nChildren) for i in xrange(nChildren))
    parent = generateNode(graph)
    
    for child in children:
        graph.add_edge(parent, child)
    
    return parent
    
def main():
    g = networkx.DiGraph()
    
    with Timer('Tree Generation'):
        root = generateTree(g, 4, 127)
        
    print 'Number of nodes: ', nodeNumber
    
    with Timer('Tree Traversal'):
        nNodes = sum(1 for n in networkx.dfs_postorder_nodes(g, root))
    
    print 'Number of nodes traversed: ', nNodes
    #print g.node
    
    
    
