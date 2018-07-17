from shutil import copyfile
import os, csv, sys, numpy

DATA_FOLDER = sys.argv[1]
OUT_FOLDER = sys.argv[2]

fracs = {}
for fileName in os.listdir(DATA_FOLDER):
    with open(os.path.join(DATA_FOLDER,fileName), 'r') as inFile:
        csvFile = csv.DictReader(inFile)
        noChange = 0
        didChange = 0
        for row in csvFile:
            if row['L-NoChange'] == '1.0' and row['L-DidChange'] == '0.0':
                noChange += 1
            elif row['L-NoChange'] == '0.0' and row['L-DidChange'] == '1.0':
                didChange += 1
            else:
                raise hell #this should never happen
        fracs[fileName] = didChange / (didChange + noChange)

os.makedirs(OUT_FOLDER, exist_ok=True)

fracslist = list(fracs.values())
mean = numpy.mean(fracslist)
std = numpy.std(fracslist)
for fileName in os.listdir(DATA_FOLDER):
    if fracs[fileName] <= mean+std:
        fullPath = os.path.join(DATA_FOLDER,fileName)
        copyfile(fullPath, os.path.join(OUT_FOLDER,fileName))