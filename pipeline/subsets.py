import os, sys, json

dataDir = sys.argv[1]
outDir = os.path.join(dataDir, 'categories')
os.mkdir(outDir)
finalDir = os.path.join(outDir, 'final')
os.mkdir(finalDir)
consecutiveDir = os.path.join(outDir, 'consecutive')
os.mkdir(consecutiveDir)

pairsFile = os.path.join(dataDir, 'goodPairs.jsonl')
with open(pairsFile, 'r') as pairsList:
    for line in pairsList:
        pair = json.loads(line)
        csvFilename = "%04d" % pair['index']+'.csv'
        src = os.path.join(dataDir, 'foo', 'blah+context+slice+size', csvFilename)
        if pair['isFinal']:
            os.symlink(src, os.path.join(finalDir, csvFilename))
        if pair['isConsecutive']:
            os.symlink(src, os.path.join(consecutiveDir, csvFilename))
