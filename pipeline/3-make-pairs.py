### Create pairs of broken and fixed code
import sys, os, json, ast

DATA_FOLDER=sys.argv[1]
OUT_FOLDER=os.path.join(DATA_FOLDER, 'pairs')

universalCounter = 0

def convertSlice(slice):
    '''Converts " (3,1)" to [3,1,3,1] and " (3,1)-(5,4)" to [3,1,5,4]'''
    return [convertSpan(span) for span in slice]

def convertSpan(span):
    span = span.strip().split("-")
    if len(span) == 1:
        span.append(span[0])
    [(a,b), (c,d)] = [ast.literal_eval(x) for x in span]
    return [a,b,c,d]

def spanToTuple(s):
    '''Converts "span_5_10_6_14" to "(5,10,6,14)"'''
    return tuple([int(n) for n in s.split("_")[1:]])

def findTriple(slicerOutput):
    '''Returns the important triple in slicer output since it sometimes outputs
    extra stuff'''
    outLines = slicerOutput.split('\n')
    for line in outLines:
        if line[0] == '(': #TODO harden this
            return ast.literal_eval(line)

os.mkdir(OUT_FOLDER)
for fileName in os.listdir(DATA_FOLDER):
    fullPath = os.path.join(DATA_FOLDER,fileName)
    if os.path.isdir(fullPath):
        continue
    pairs = []
    with open(fullPath, 'r') as logFile:
        badProgs = []
        for line in logFile:
            dct = json.loads(line)
            if "PF_exitPipelineReason" in dct:
                # if dct["PF_exitPipelineReason"] != 'Does not parse':
                    # slicer or ANFer crashed or timed out
                    # TODO Since we can't be sure if this program was good or bad,
                    # throw out unpaired bad programs before this point
                    # badProgs = []
                continue
            newProg = dct['user_script']
            (result, types, slice) = findTriple(dct['PF_slicerOutput'])
            if result == None:
                # found a good program!
                for (badProg, badSlice, badTypes, badExceptionSpan) in badProgs:
                    varTypes = {}
                    spanTypes = {}
                    for key in badTypes:
                        if key[:5] == "span_":
                            newKey = spanToTuple(key)
                            spanTypes[str(newKey)] = badTypes[key]
                        else:
                            varTypes[key] = badTypes[key]
                    # varSlice = []
                    # spanSlice = []
                    # for ident in badSlice:
                    #     if ident[:5] == "span_":
                    #         spanSlice.append(spanToTuple(ident))
                    #     else:
                    #         varSlice.append(ident)
                    pairs.append({"bad": badProg,
                                  "fix": newProg,
                                  "index": universalCounter,
                                  "pyVersion": 3,
                                  "exceptionSpan": convertSpan(badExceptionSpan),
                                  # "varSlice": varSlice,
                                  "spanSlice": [convertSpan(span) for span in badSlice],
                                  "varTypes": varTypes,
                                  "spanTypes": spanTypes,})
                    universalCounter += 1
                badProgs = []
            else:
                # found a program with an error slice
                # anf_lines = dct["anf_user_script"].split('\n')
                # slice = []
                # for lineNum in result:
                #     slice.append(anf_lines[lineNum-1].split(" = ")[0].strip())
                badProgs.append((newProg, slice, types, result))
    if len(pairs) > 0:
        with open(os.path.join(OUT_FOLDER,fileName), 'w') as outFile:
            for pair in pairs:
                outFile.write(json.dumps(pair) + "\n")
