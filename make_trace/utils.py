import os, json

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
                func(dct, outFile)
                outFile.write(json.dumps(dct) + '\n')
    os.remove(fullPath)
