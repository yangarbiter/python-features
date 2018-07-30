import ast
import scipy.cluster.hierarchy
from scipy.spatial.distance import pdist
import zss

from sklearn.cluster import AgglomerativeClustering

def ast_children(node):
    return list(ast.iter_child_nodes(node))

def ast_label(node):
    return type(node)

def label_distance(a, b):
    if a == b:
        return 0
    else:
        return 1

def ast_distance(a, b):
    return zss.simple_distance(a, b, ast_children, ast_label, label_distance)

# Takes a 1 element list containing the ast to compare
def metric(a, b):
    return ast_distance(a[0], b[0])

def cluster(asts, t):
    distance_matrix = pdist(list(map(lambda x: [x], asts)), metric)
    linkage = scipy.cluster.hierarchy.linkage(distance_matrix, method='single')
    return scipy.cluster.hierarchy.fcluster(linkage, t, criterion='distance')

# Takes a list of python source strings and produces an array where element i is
# the cluster number of the string with index i in the iterable that was passed
# in.
#
# strings: Iterable of python source strings
# t: Max edit distance for elements in the same cluster
def cluster_strings(strings, t):
    asts = map(ast.parse, strings)
    return cluster(asts, t)
