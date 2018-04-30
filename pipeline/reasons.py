### Counts how many lines pass and how many fail for each reason
import sys, collections, ast
from utils import doForNonDirs

DATA_FOLDER=sys.argv[1]

def findTriple(slicerOutput):
    '''Returns the important triple in slicer output since it sometimes outputs
    extra stuff'''
    outLines = slicerOutput.split('\n')
    for line in outLines:
        if line[0] == '(': #TODO harden this
            return ast.literal_eval(line)

def handleFile(dct, results):
    if "PF_exitPipelineReason" not in dct:
        foo = findTriple(dct["PF_slicerOutput"])[0]
        extra = "pass" if foo == None else "fail"
        results["(success) - "+extra] += 1
    else:
        results[dct['PF_exitPipelineReason'][0]] += 1

results = collections.defaultdict(int)
doForNonDirs(DATA_FOLDER, handleFile, results)
print(results)
