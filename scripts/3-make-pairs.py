### Create pairs of broken and fixed code
import sys, os, json, ast, subprocess

DATA_FOLDER=sys.argv[1]
OUT_FOLDER=os.path.join(DATA_FOLDER, 'pairs')

universalCounter = 0

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
                result = dct['__result']
                newProg = dct['user_script']
                types = dct['__types']
                if result == "None\n":
                    # found a good program!
                    for (badProg, badSlice, badTypes) in badProgs:
                        pairs.append({"bad": badProg,
                                      "fix": newProg,
                                      "index": universalCounter,
                                      "pyVersion": 3,
                                      "slice": list(ast.literal_eval(badSlice)),
                                      "badTypes": ast.literal_eval(badTypes.strip()),
                                      "fixTypes": ast.literal_eval(types.strip())})
                        universalCounter += 1
                    badProgs = []
                elif result[0] == '{':
                    # found a program with an error slice
                    badProgs.append((newProg, result, types))
                else:
                    # slicer crashed or timed out
                    # Since we can't be sure if this program was good or bad,
                    # throw out unpaired bad programs before this point
                    badProgs = []
        except Exception as e:
            print(e)
    if len(pairs) > 0:
        with open(os.path.join(OUT_FOLDER,fileName), 'w') as outFile:
            for pair in pairs:
                outFile.write(json.dumps(pair) + "\n")
