# TODO handle incomplete input (probably throw out the submission)

# Annotates each submission with whether it runs to completion without crash
import sys, os, json, subprocess

TIMEOUT=2  # max time per program (sec)
# DATA_FOLDER='data/filtered/py3-web_exec/'
DATA_FOLDER='data/filtered/py3-web_exec/annotated/slices-ok/updated-with-anf'
# OUT_FOLDER='data/filtered/py3-web_exec/annotated/'
OUT_FOLDER='data/filtered/py3-web_exec/annotated/slices-ok/updated-with-anf/typed'

for fileName in os.listdir(DATA_FOLDER):
    fullPath = os.path.join(DATA_FOLDER,fileName)
    if os.path.isdir(fullPath):
        continue
    with open(fullPath, 'r') as logFile:
        with open(os.path.join(OUT_FOLDER,fileName), 'w') as outFile:
            print(fileName)
            for line in logFile:
                dct = json.loads(line)
                if dct['__parses'] != "yes":
                    err = "<does not parse>"
                elif dct['__result'][:2] == 'Tr':
                    err = "slicer failure"
                else:
                    try:
                        x = subprocess.check_output(["python", "process_json.py", line], stderr=subprocess.STDOUT, timeout=TIMEOUT)
                        err = x.decode("utf-8")
                    except subprocess.CalledProcessError as e:
                        err = e.output.decode("utf-8") #.split("\n")[-2]
                        # err = "ERROR"
                    except subprocess.TimeoutExpired:
                        err = "<timeout (%d seconds)>" % TIMEOUT
                # print(err)
                # sys.stdout.flush()
                dct['__types_anf'] = err
                outFile.write(json.dumps(dct) + '\n')
    os.remove(os.path.join(DATA_FOLDER,fileName))


# Invocation example:
# docker build -t python-munge .
# docker run -v /Users/benjamin/LessTemporaryDownloads/may-to-july-2017-server-logs/data:/data python-munge

### Since docker appears to sometimes cut off early (possibly because it's not
### flushing stdout before exiting?), ALL code above intended to be run in
### docker should end with this.
print("done")
sys.stdout.flush()
