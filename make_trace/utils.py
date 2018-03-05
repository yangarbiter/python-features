import os
from multiprocessing import Pool

def doForNonDirs(folder, func, loud=False):
    for fileName in os.listdir(folder):
        fullPath = os.path.join(folder,fileName)
        if os.path.isdir(fullPath):
            continue
        if loud:
            print(fileName)
        with open(fullPath, 'r') as logFile:
            func(fileName, logFile)

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
