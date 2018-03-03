### Create pairs of broken and fixed code
import sys, os, json, ast, subprocess

DATA_FOLDER=sys.argv[1]
OUT_FOLDER=os.path.join(DATA_FOLDER, 'pairs')

universalCounter = 0

def spanToTuple(s):
    '''Converts "span_5_10_6_14" to "(5,10,6,14)"'''
    return tuple([int(n) for n in s.split("_")[1:]])


for fileName in os.listdir(DATA_FOLDER):
    fullPath = os.path.join(DATA_FOLDER,fileName)
    if os.path.isdir(fullPath):
        continue
    pairs = []
    with open(fullPath, 'r') as logFile:
        badProgs = []
        try:
            for line in logFile:
                dct = json.loads(line)
                if dct['__parses'] != "yes":
                    continue
                newProg = dct['user_script']
                types_pair = dct['__types_anf']
                if types_pair[0] != '(':
                    # slicer crashed or timed out
                    # Since we can't be sure if this program was good or bad,
                    # throw out unpaired bad programs before this point
                    badProgs = []
                else:
                    (result, types) = ast.literal_eval(types_pair.strip())
                    if result == None:
                        # found a good program!
                        for (badProg, badSlice, badTypes) in badProgs:
                            varTypes = {}
                            spanTypes = {}
                            for key in badTypes:
                                if key[:5] == "span_":
                                    newKey = spanToTuple(key)
                                    spanTypes[str(newKey)] = badTypes[key]
                                else:
                                    varTypes[key] = badTypes[key]
                            varSlice = []
                            spanSlice = []
                            for ident in badSlice:
                                if ident[:5] == "span_":
                                    spanSlice.append(spanToTuple(ident))
                                else:
                                    varSlice.append(ident)
                            pairs.append({"bad": badProg,
                                          "fix": newProg,
                                          "index": universalCounter,
                                          "pyVersion": 3,
                                          "varSlice": varSlice,
                                          "spanSlice": spanSlice,
                                          "varTypes": varTypes,
                                          "spanTypes": spanTypes,})
                            universalCounter += 1
                        badProgs = []
                    elif type(result) == set:
                        # found a program with an error slice
                        anf_lines = dct["anf_user_script"].split('\n')
                        slice = []
                        for lineNum in result:
                            slice.append(anf_lines[lineNum-1].split(" = ")[0].strip())
                        badProgs.append((newProg, slice, types))
                    else:
                        raise hell # this should not happen
        except Exception as e:
            print(e)
    if len(pairs) > 0:
        with open(os.path.join(OUT_FOLDER,fileName), 'w') as outFile:
            for pair in pairs:
                outFile.write(json.dumps(pair) + "\n")
