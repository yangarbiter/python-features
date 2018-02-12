from subprocess import Popen, PIPE
import json, signal, sys, os

DATA_FOLDER = sys.argv[1]

def decodeTuple(x):
    (a,b) = x
    if a is not None:
        a = a.decode("utf-8")
    if b is not None:
        b = b.decode("utf-8")
    return (a,b)

i = 0

#https://stackoverflow.com/questions/1112343/how-do-i-capture-sigint-in-python
def signal_handler(signal, frame):
    print(i)
    sys.exit(0)
signal.signal(signal.SIGINT, signal_handler)

with open("failPairs.jsonl", 'w') as failFile:
    with open("goodPairs.jsonl", 'w') as goodFile:
        for fileName in os.listdir(DATA_FOLDER):
            print(fileName)
            with open(os.path.join(DATA_FOLDER,fileName), 'r') as inFile:
                lines = inFile.readlines()
                for line in lines:
                    dct = json.loads(line.strip())
                    with open("temp.json", 'w') as outFile:
                        outFile.write(line)
                    p = Popen("stack exec -- generate-features --source temp.json --features op+context --out blah".split(), stdout=PIPE, stderr=PIPE)
                    output = p.communicate()
                    # print(output)
                    # print(p.returncode)
                    dct['__retCode'] = p.returncode # for debugging
                    dct['__output'] = decodeTuple(output) # for debugging
                    if p.returncode != 0:
                        failFile.write(json.dumps(dct)+'\n')
                        failFile.flush()
                    else:
                        goodFile.write(json.dumps(dct)+'\n')
                        goodFile.flush()
                    i += 1
print(i)
