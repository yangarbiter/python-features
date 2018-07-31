### Create pairs of broken and fixed code
import sys, os, json, ast

DATA_FOLDER=sys.argv[1]
OUT_FOLDER=sys.argv[2]

universalCounter = 0

def spanToTuple(s):
    '''Converts "span_5_10_6_14" to "(5,10,6,14)"'''
    return tuple([int(n) for n in s.split("_")[1:]])

def findTuple(slicerOutput):
    '''Returns the important tuple in slicer output since it sometimes outputs
    extra stuff'''
    outLines = slicerOutput.split('\n')
    for line in outLines:
        if line[0] == '(': #TODO harden this
            return ast.literal_eval(line)

os.makedirs(OUT_FOLDER, exist_ok=True)
for fileName in os.listdir(DATA_FOLDER):
    try:
        fullPath = os.path.join(DATA_FOLDER,fileName)
        with open(fullPath, 'r') as logFile:
            lines = [json.loads(line) for line in logFile]
            lines.sort(key=lambda x: x['serverTimeUTC'])
        pairs = []
        badProgs = []
        for dct in lines:
            if "PF_exitPipelineReason" in dct:
                # if dct["PF_exitPipelineReason"] != 'Does not parse':
                    # slicer or ANFer crashed or timed out
                    # TODO Since we can't be sure if this program was good or bad,
                    # throw out unpaired bad programs before this point
                    # badProgs = []
                continue
            newProg = dct['user_script']
            tup = findTuple(dct['PF_slicerOutput'])
            (result, types, slice, msg, UD_1, _) = tup
            if msg != None:
                msg = msg.split(":")[0]
            # debugDict[msg] += 1
            if result == None:
                # found a good program!
                if len(badProgs) > 0:
                    for ((badProg, badSlice, badTypes, badExceptionSpan, badMsg, badUD), isConsecutive) in zip(badProgs, [False]*(len(badProgs)-1)+[True]):
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
                                    "exceptionSpan": badExceptionSpan,
                                    # "varSlice": varSlice,
                                    "spanSlice": badSlice,
                                    "varTypes": varTypes,
                                    "spanTypes": spanTypes,
                                    "errMsg": badMsg,
                                    "ud": {str(k):list(v) for (k,v) in badUD.items()},
                                    "isConsecutive": isConsecutive,
                                    "isFinal": False})
                        universalCounter += 1
                    badProgs = []
            else:
                # found a program with an error slice
                lines = dct['PF_anf_user_script'].split('\n')
                badProgs.append((newProg, slice, types, result, msg, UD_1))
        if len(pairs) > 0:
            pairs[-1]["isFinal"] = True
            with open(os.path.join(OUT_FOLDER,fileName), 'w') as outFile:
                for pair in pairs:
                    outFile.write(json.dumps(pair) + "\n")
    except json.decoder.JSONDecodeError:
        print("JSONDecodeError in: %s" % fileName)

# print(debugDict)