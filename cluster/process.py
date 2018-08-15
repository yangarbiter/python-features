from parse import python_sources_from_glob
import sys
import cluster
import random
import stringify
import ast
import json
import numpy as np

def read_python():
    print('Building list of programs...')
    strings = python_sources_from_glob(sys.argv[1])

    print('Transforming...')
    asts = map(ast.parse, strings)
    return list(map(stringify.transform_python, asts))

def read_ocaml():
    decoder = json.JSONDecoder()
    
    strings = []
    print('Building list of transformed programs...')
    with open('ocaml-json') as f:
        for line in f:
            obj = decoder.decode(line)
            strings.append(stringify.transform_ocaml(obj))

    return strings

data_name = ''

if len(sys.argv) == 1:
    data_name = 'ocaml'
    transformeds = read_ocaml()
elif len(sys.argv) == 2:
    data_name = 'python'
    transformeds = read_python()
else:
    print('0 args for ocaml, 1 arg for python')
    assert(false)

print('Sampling programs...')
indices = random.sample(list(range(len(transformeds))), 5000)
np.array(indices).tofile(data_name + '_indices', sep='\n')
selection = [transformeds[i] for i in indices]

print('Clustering...')
cluster.cluster_strings(selection, data_name)

print('Done clustering. Cluster counts output to %s_cluster_counts_*.' % data_name)
