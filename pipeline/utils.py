import os, json
from itertools import repeat
from multiprocessing import Pool

import sys

def doForNonDirs(dataFolder, func, outArg, loud=False):
    for fileName in os.listdir(dataFolder):
        fullPath = os.path.join(dataFolder,fileName)
        if os.path.isdir(fullPath):
            continue
        if loud:
            print(fileName)
        with open(fullPath, 'r') as logFile:
            for line in logFile:
                try:
                    dct = json.loads(line)
                    func(dct, outArg)
                except json.decoder.JSONDecodeError:
                    print("WEIRD JSON DECODE IN FILE: %s" % logFile)

def doFunc(func, fileName, dataFolder, outFolder, ignoredFiles):
    fullPath = os.path.join(dataFolder,fileName)
    if os.path.isdir(fullPath):
        return
    if fileName in ignoredFiles:
        return
    try:
        with open(fullPath, 'r') as logFile:
            with open(os.path.join(outFolder,fileName), 'w') as outFile:
                for line in logFile:
                    dct = json.loads(line)
                    if "PF_exitPipelineReason" in dct:
                        outFile.write(line)
                        continue
                    func(line, dct, outFile)
                    outFile.write(json.dumps(dct) + '\n')
    except Exception: # temporary; trying to catch one weird file that breaks the slice pass
        print("WEIRD EXCEPTION IN FILE: %s" % fileName)

    # os.remove(fullPath)

def doPass(func, dataFolder, outFolder):
    os.makedirs(outFolder, exist_ok=True)
    ignoredFiles = os.listdir(outFolder)
    argsIter = zip(os.listdir(dataFolder), repeat(dataFolder), repeat(outFolder), repeat(ignoredFiles))
    with Pool() as p:
        p.starmap(func, argsIter)
