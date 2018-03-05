import sys

from filterPass import filterPass
from anfPass import anfFunc
from slicePass import sliceFunc

passName = sys.argv[1]
if passName == "filter":
    filterPass(sys.argv[2], sys.argv[3])
elif passName == "anf":
    doPass(anfFunc, sys.argv[2], 'updated-with-anf')
elif passName == "slice":
    doPass(sliceFunc, sys.argv[2], 'sliced')

def doPass(func, dataFolder, outSuffix):
    outFolder = os.path.join(dataFolder, outSuffix)
    os.mkdir(outFolder)
    argsIter = zip(os.listdir(dataFolder), repeat(dataFolder), repeat(outFolder))
    with Pool() as p:
        p.map(func, os.listdir(dataFolder))
