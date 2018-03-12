import os, json
from itertools import repeat
from multiprocessing import Pool

def doForNonDirs(dataFolder, func, outArg, loud=False):
    for fileName in os.listdir(dataFolder):
        fullPath = os.path.join(dataFolder,fileName)
        if os.path.isdir(fullPath):
            continue
        if loud:
            print(fileName)
        with open(fullPath, 'r') as logFile:
            for line in logFile:
                dct = json.loads(line)
                func(dct, outArg)

def doFunc(func, fileName, dataFolder, outFolder):
    fullPath = os.path.join(dataFolder,fileName)
    if os.path.isdir(fullPath):
        return
    with open(fullPath, 'r') as logFile:
        with open(os.path.join(outFolder,fileName), 'w') as outFile:
            for line in logFile:
                dct = json.loads(line)
                if "PF_exitPipelineReason" in dct:
                    outFile.write(line)
                    continue
                func(line, dct, outFile)
                outFile.write(json.dumps(dct) + '\n')
    os.remove(fullPath)

def doPass(func, dataFolder, outSuffix):
    outFolder = os.path.join(dataFolder, outSuffix)
    # os.mkdir(outFolder)
    argsIter = zip(os.listdir(dataFolder), repeat(dataFolder), repeat(outFolder))
    with Pool() as p:
        p.starmap(func, argsIter)
