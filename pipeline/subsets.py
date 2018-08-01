import os, sys, json

dataDir = sys.argv[1]

outDir = os.path.join(dataDir, 'subsets')
os.mkdir(outDir)

finalsFilename = os.path.join(outDir, 'finals.txt')
consecutivesFilename = os.path.join(outDir, 'consecutives.txt')
pairsFile = os.path.join(dataDir, 'goodPairs.jsonl')
with open(pairsFile, 'r') as pairsList, open(finalsFilename, 'w') as finalsFile, open(consecutivesFilename, 'w') as consecutivesFile:
    for line in pairsList:
        pair = json.loads(line)
        csvFilename = "%04d" % pair['index']+'.csv'
        if pair['isFinal']:
            finalsFile.write(csvFilename+'\n')
        if pair['isConsecutive']:
            consecutivesFile.write(csvFilename+'\n')
