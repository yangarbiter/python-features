import csv, sys, os
from shutil import copyfile

DATA_FOLDER = sys.argv[1]
OUT_FOLDER = sys.argv[2]

goodCsvsFolder = os.path.join(OUT_FOLDER, 'goodCsvs')
failCsvsFolder = os.path.join(OUT_FOLDER, 'failCsvs')

os.mkdir(goodCsvsFolder)
os.mkdir(failCsvsFolder)
files = 0
runningTotal = 0
for fileName in os.listdir(DATA_FOLDER):
    fullPath = os.path.join(DATA_FOLDER,fileName)
    noChange = 0
    didChange = 0
    with open(fullPath, 'r') as csvFile:
        foo = csv.reader(csvFile)
        skippedHeader = False
        for row in foo:
            if not skippedHeader:
                skippedHeader = True
                continue
            if row[1] == '1.0' and row[2] == '0.0':
                noChange += 1
            elif row[1] == '0.0' and row[2] == '1.0':
                didChange += 1
            else:
                raise hell #this should never happen
    # runningTotal += didChange / (noChange + didChange)
    files += 1
    if noChange > 0 and didChange > 0:
        copyfile(fullPath, os.path.join(goodCsvsFolder,fileName))
    else:
        copyfile(fullPath, os.path.join(failCsvsFolder,fileName))
# print(runningTotal, files, runningTotal / files)
