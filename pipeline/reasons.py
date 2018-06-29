### Counts how many lines pass and how many fail for each reason
import sys, collections, ast
import json

outfile = open('temp.jsonl', 'w')

def findTriple(slicerOutput):
    '''Returns the important triple in slicer output since it sometimes outputs
    extra stuff'''
    outLines = slicerOutput.split('\n')
    for line in outLines:
        if line[0] == '(': #TODO harden this
            return ast.literal_eval(line)

def canonicalize(err):
    try:
        if "KeyError" in err and "heap" not in err and "None" not in err:
            return "[some other KeyError]"
        else:
            return err
    except Exception:
        print(err)

def handleFile(dct, results):
    if "PF_exitPipelineReason" not in dct:
        triple = findTriple(dct["PF_slicerOutput"])
        extra = "pass" if triple[0] == None else "fail"
        results["(success) - "+extra] += 1
    else:
        if dct['PF_exitPipelineReason'][1] == "Slicer error":
            reason = dct['PF_exitPipelineReason'][2].split('\n')[-2]
        else:
            reason = dct['PF_exitPipelineReason'][1]
        results[canonicalize(reason)] += 1

results = collections.defaultdict(int)
for line in sys.stdin:
    handleFile(json.loads(line), results)
sortedResultList = sorted(results.items(), key=lambda e: e[1], reverse=True)
total = sum([val for (key, val) in sortedResultList])
for key, value in sortedResultList:
    print("%s: %s (%.1f%%)" % (key, value, value*100/total))
