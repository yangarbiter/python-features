import sys
import json
import glob
import cluster

decoder = json.JSONDecoder()

paths = glob.glob(sys.argv[1])

strings = []

print('Building list of programs...')
for path in paths:
    with open(path) as file:
        # Get the last element
        last = next(file, "")
        try:
            obj = decoder.decode(last)
            source = obj['fix']
            strings.append(source)
        except Exception as e:
            print(e)    

print('Clustering...')
clustering = cluster.cluster_strings(strings, int(sys.argv[2]))

print('Done clustering. Cluster count is %d' % max(clustering))
