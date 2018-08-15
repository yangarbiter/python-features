import numpy as np
import os
from scipy.spatial.distance import pdist
from scipy.cluster.hierarchy import linkage, fcluster
import stringify

def distance_matrix(strings):
    return pdist(list(map(lambda x: [x], strings)), stringify.metric)

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

    for method in ['single', 'average', 'complete']:
        print('Computing linkage (%s)...' % method)
        link = linkage(dmat, method=method)
        link.tofile(data_name + '_progress/link_' + method)

        print('Computing clusters (%s)...' % method)
        cluster_counts = []
        for i in range(201):
            t = i / 100
            cluster_counts.append(fcluster(link, t).max())
            np.array(cluster_counts).tofile(data_name + '_cluster_counts_' + method, sep='\n')
    
