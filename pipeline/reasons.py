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

# def canonicalize(err):
#     if "KeyError" in err and "heap" not in err and "None" not in err:
#         return "[some other KeyError]"
#     else:
#         return err

def handleFile(dct, results):
    if "PF_exitPipelineReason" not in dct:
        foo = findTriple(dct["PF_slicerOutput"])[0]
        extra = "pass" if foo == None else "fail"
        results["(success) - "+extra] += 1
    else:
        # if 'Slicer error' == dct['PF_exitPipelineReason'][0]:
        #     x = dct['PF_exitPipelineReason'][1].split('\n')[-2]
        #     if canonicalize(x) == "[some other KeyError]" and len(dct['user_script'].split('\n')) < 7:
        #         print('\n\n--------------------------\n\n')
        #         print(dct['user_script'])
        #     results[canonicalize(x)] += 1
        # else:
            results[dct['PF_exitPipelineReason'][0]] += 1

results = collections.defaultdict(int)
doForNonDirs(DATA_FOLDER, handleFile, results)
sortedResultList = sorted(results.items(), key=lambda e: e[1], reverse=True)
total = sum([val for (key, val) in sortedResultList])
for key, value in sortedResultList:
    print("%s: %s (%.1f%%)" % (key, value, value*100/total))
