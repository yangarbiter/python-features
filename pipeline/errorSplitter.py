import os, sys, json, csv
import progressbar

bar = progressbar.progressbar.ProgressBar()

dataDir = sys.argv[1]

inDir = os.path.join(dataDir, 'outliersRemoved')
outDir = os.path.join(dataDir, 'subsets')

for filename in bar(os.listdir(inDir)):
    src = os.path.join(inDir, filename)
    with open(src) as f:
        x = csv.DictReader(f)
        err = next(x)['F-PythonMsg']
        errDir = os.path.join(outDir, err)
        if not os.path.exists(errDir):
            os.mkdir(errDir)
        os.link(src, os.path.join(errDir, filename))
