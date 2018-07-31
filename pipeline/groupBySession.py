import os.path, os
import sys
import json

OUT_FOLDER = sys.argv[1]
os.makedirs(OUT_FOLDER, exist_ok=True)

for line in sys.stdin:
    dct = json.loads(line)
    if 'PF_exitPipelineReason' in dct:
        continue
    with open(os.path.join(OUT_FOLDER,dct['session_uuid']), 'a') as outFile:
        outFile.write(json.dumps(dct)+"\n")
