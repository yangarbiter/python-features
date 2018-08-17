import sys

paths = sys.argv[1:]
cluster_counts = {}

for path in paths:
    with open(path) as f:
        cluster_counts[path] = []
        for x in f:
            cluster_counts[path].append(x.strip())

with open('cluster_counts.csv', 'wt') as out_file:
    out_file.write('threshold')

    for path in paths:
        out_file.write(',')
        out_file.write(path)
    
    for i in range(200):
        out_file.write(str(i / 100.0))
        for path in paths:
            out_file.write(',')
            out_file.write(cluster_counts[path][i])
        out_file.write('\n')
