import ast
import scipy.cluster.hierarchy
from scipy.spatial.distance import pdist
import zss

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

def distance_matrix(asts):
    return pdist(list(map(lambda x: [x], asts)), metric)

def linkage(d_matrix):
    return scipy.cluster.hierarchy.linkage(d_matrix, method='single')

def cluster(link, t):
    return scipy.cluster.hierarchy.fcluster(link, t, criterion='distance')

# Takes a list of python source strings and produces an array where element i is
# the cluster number of the string with index i in the iterable that was passed
# in.
#
# strings: Iterable of python source strings
# t: Max edit distance for elements in the same cluster
def cluster_strings(strings, t):
    print('Parsing strings...')
    asts = map(ast.parse, strings)

    print('Computing distance matrix...')
    dmat = distance_matrix(asts)

    print('Computing linkage...')
    link = linkage(dmat)

    print('Computing clusters...')
    return cluster(link, t)
