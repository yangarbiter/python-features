import scipy.cluster.hierarchy
import numpy as np
import os
from scipy.spatial.distance import pdist
import stringify

def distance_matrix(strings):
    return pdist(list(map(lambda x: [x], strings)), stringify.metric)

def linkage(d_matrix):
    return scipy.cluster.hierarchy.linkage(d_matrix, method='single')

def cluster(link, t):
    return scipy.cluster.hierarchy.fcluster(link, t)

# Takes a list of python source strings and produces an array where element i is
# the cluster number of the string with index i in the iterable that was passed
# in.
#
# strings: Iterable of python source strings
# t: Max edit distance for elements in the same cluster
def cluster_strings(strings, data_name):
    # Used to store intermediate things in case a bug causes crashes
    try:
        os.mkdir(data_name + '_progress')
    except:
        # Assume any error means the directory exists
        pass
    
    print('Computing distance matrix...')
    dmat = distance_matrix(strings)
    dmat.tofile(data_name + '_progress/dmat')

    print('Computing linkage...')
    link = linkage(dmat)
    link.tofile(data_name + '_progress/link')

    print('Computing clusters...')
    cluster_counts = []
    for i in range(200):
        t = i / 100
        cluster_counts.append(cluster(link, t).max())
    np.array(cluster_counts).tofile(data_name + '_cluster_counts', sep='\n')

def cluster_only():
    link = np.fromfile(data_name + '_progress/link').reshape(-1, 4)
    cluster_counts = []
    for i in range(200):
        t = i / 100
        cluster_counts.append(cluster(link, t).max())
    np.array(cluster_counts).tofile(data_name + '_cluster_counts', sep='\n')
    
