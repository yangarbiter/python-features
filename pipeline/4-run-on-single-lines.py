from subprocess import run, PIPE
import json, sys, os

DATA_FOLDER = sys.argv[1]
GENERATE_FEATURES_BIN = ".stack-work/install/x86_64-osx/lts-8.14/8.0.2/bin/generate-features"

with open("failPairs.jsonl", 'w') as failFile:
    with open("goodPairs.jsonl", 'w') as goodFile:
        for fileName in os.listdir(DATA_FOLDER):
            # print(fileName)
            with open(os.path.join(DATA_FOLDER,fileName), 'r') as inFile:
                lines = inFile.readlines()
                for line in lines:
                    dct = json.loads(line.strip())
                    with open("temp.json", 'w') as outFile:
                        outFile.write(line)
                    p = run([GENERATE_FEATURES_BIN]+"--source temp.json --features op --out blah".split(),
                            stdout=PIPE, stderr=PIPE, encoding="utf-8", errors="strict")
                    # print(p.stdout, p.stderr, p.returncode)
                    dct['PF_retCode'] = p.returncode # for debugging
                    dct['PF_output'] = (p.stdout, p.stderr) # for debugging
                    if p.returncode != 0:
                        failFile.write(json.dumps(dct)+'\n')
                        failFile.flush()
                    else:
                        goodFile.write(json.dumps(dct)+'\n')
                        goodFile.flush()
