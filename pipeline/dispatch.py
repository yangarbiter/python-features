import sys

from filterPass import filterPass
from anfPass import anfFunc
from slicePass import sliceFunc
from utils import doPass

passName = sys.argv[1]
if passName == "filter":
    filterPass(sys.argv[2], sys.argv[3])
elif passName == "anf":
    doPass(anfFunc, sys.argv[2], 'updated-with-anf')
elif passName == "slice":
    doPass(sliceFunc, 'data', 'sliced')
elif passName == "all":
    filterPass(sys.argv[2], sys.argv[3])
    doPass(anfFunc, sys.argv[3]+'/py3-web_exec', 'updated-with-anf')
    doPass(sliceFunc, sys.argv[3]+'/py3-web_exec/updated-with-anf', 'sliced')
