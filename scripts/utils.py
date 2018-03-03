import os

def doForNonDirs(folder, func, loud=False):
    for fileName in os.listdir(folder):
        fullPath = os.path.join(folder,fileName)
        if os.path.isdir(fullPath):
            continue
        if loud:
            print(fileName)
        with open(fullPath, 'r') as logFile:
            func(fileName, logFile)
