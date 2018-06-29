from collections import defaultdict
import os.path, os
import sys
import json

OUT_FOLDER = sys.argv[1]
os.makedirs(OUT_FOLDER, exist_ok=True)

result = defaultdict(list)
for line in sys.stdin:
    dct = json.loads(line)
    if 'PF_exitPipelineReason' in dct:
        continue
    result[dct['session_uuid']].append(dct)

for (uuid, dctlist) in result.items():
    if len(dctlist) < 2:
        continue
    dctlist.sort(key=lambda x: x['serverTimeUTC'])
    with open(os.path.join(OUT_FOLDER,uuid), 'w') as outFile:
        for dct in dctlist:
            outFile.write(json.dumps(dct)+"\n")
