from subprocess import run, PIPE
import json, sys, os

DATA_FOLDER = sys.argv[1]
OUT_FOLDER = sys.argv[2]
with open('.env', 'r') as envFile:
    stackBinDir = envFile.readlines()[0]
GENERATE_FEATURES_BIN = os.path.join(stackBinDir, "generate-features")

with open(OUT_FOLDER+"/failPairs.jsonl", 'w') as failFile, open(OUT_FOLDER+"/goodPairs.jsonl", 'w') as goodFile:
    for fileName in os.listdir(DATA_FOLDER):
        with open(os.path.join(DATA_FOLDER,fileName), 'r') as inFile:
            lines = inFile.readlines()
            for line in lines:
                dct = json.loads(line.strip())
                with open("temp.json", 'w') as tempFile:
                    tempFile.write(line)
                cmd = "--source temp.json --out %s" % (OUT_FOLDER+"/temp")
                p = run([GENERATE_FEATURES_BIN]+cmd.split(),
                        stdout=PIPE, stderr=PIPE, encoding="utf-8", errors="strict")
                # dct['PF_output'] = (p.stdout, p.stderr) # for debugging
                outFile = failFile if p.returncode != 0 else goodFile
                outFile.write(json.dumps(dct)+'\n')
