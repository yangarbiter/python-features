### Counts how many lines pass and how many fail for each reason
import sys, collections
from utils import doForNonDirs

DATA_FOLDER=sys.argv[1]

def handleFile(dct, results):
    if "PF_exitPipelineReason" not in dct:
        results["(success)"] += 1
        # print(dct["PF_slicerOutput"])
    else:
        results[dct['PF_exitPipelineReason'][0]] += 1

results = collections.defaultdict(int)
doForNonDirs(DATA_FOLDER, handleFile, results)
print(results)
