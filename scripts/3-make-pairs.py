### Create pairs of broken and fixed code
import sys, os, json, ast, subprocess

DATA_FOLDER=sys.argv[1]
OUT_FOLDER=os.path.join(DATA_FOLDER, 'pairs')

universalCounter = 0

def doStuff(fileName):
    global universalCounter
    pairs = []
    with open(os.path.join(DATA_FOLDER,fileName), 'r') as logFile:
        badProgs = []
        try:
            for line in logFile:
                dct = json.loads(line)
                if dct['__parses'] != "yes":
                    continue
                result = dct['__result']
                newProg = dct['user_script']
                if result == "None\n":
                    # found a good program!
                    for (prog, slice) in badProgs:
                        pairs.append({"bad": prog,
                                      "fix": newProg,
                                      "index": universalCounter,
                                      "pyVersion": 3,
                                      "slice": list(ast.literal_eval(slice))})
                        universalCounter += 1
                    badProgs = []
                elif result[0] == '{':
                    # found a program with an error slice
                    badProgs.append((newProg, dct['__result'])) #TODO finish including error slice in pair
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

for fileName in os.listdir(DATA_FOLDER):
    if fileName == "pairs":
        continue
    doStuff(fileName)
    # os.rename(DATA_FOLDER+fileName, JUNK_DRAWER+fileName)
