import sys

from anfPass import anfFunc
from slicePass import sliceFunc
from utils import doPass

passName = sys.argv[1]
if passName == "anf":
    doPass(anfFunc, sys.argv[2], 'updated-with-anf')
elif passName == "slice":
    doPass(sliceFunc, 'data', 'sliced')
