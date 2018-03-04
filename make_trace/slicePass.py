### Annotates each submission with whether it runs to completion without crash
# TODO handle incomplete input (probably throw out the submission)
import sys, os, json
from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
from multiprocessing import Pool

SLI_dataFolder = ""
SLI_outFolder = ""

TIMEOUT=2  # max time per program (sec)

# Invocation example:
# docker build -t python-munge .
# docker run -v /Users/benjamin/LessTemporaryDownloads/may-to-july-2017-server-logs/data:/data python-munge

# DATA_FOLDER='data/filtered/py3-web_exec/updated-with-anf'
# OUT_FOLDER='data/filtered/py3-web_exec/updated-with-anf/typed'

def func(fileName):
    fullPath = os.path.join(SLI_dataFolder,fileName)
    if os.path.isdir(fullPath):
        return
    with open(fullPath, 'r') as logFile:
        with open(os.path.join(SLI_outFolder,fileName), 'w') as outFile:
            print(fileName)
            for line in logFile:
                dct = json.loads(line)
                if "PF_exitPipelineReason" in dct:
                    outFile.write(line)
                    continue
                try:
                    # running as separate process for ease of TIMEOUT
                    x = run(["python3", "process_raw.py", line], stdout=PIPE, stderr=PIPE, encoding="utf-8", errors="strict", timeout=TIMEOUT, check=True)
                    dct["PF_slicerOutput"] = x.stdout
                except CalledProcessError as e:
                    dct['PF_exitPipelineReason'] = ("Slicer error", e.stderr)
                except TimeoutExpired:
                    dct['PF_exitPipelineReason'] = ("Slicer timeout", TIMEOUT)
                outFile.write(json.dumps(dct) + '\n')
    os.remove(os.path.join(SLI_dataFolder,fileName))

def slicePass(dataFolder):
    global SLI_dataFolder
    global SLI_outFolder
    SLI_dataFolder = dataFolder
    SLI_outFolder = os.path.join(dataFolder, 'sliced')
    # os.mkdir(SLI_outFolder)
    with Pool() as p:
        p.map(func, os.listdir(dataFolder))
    ### Since docker appears to sometimes cut off early (possibly because it's not
    ### flushing stdout before exiting?), ALL code above intended to be run in
    ### docker should end with this.
    print("done")
    sys.stdout.flush()

if __name__ == '__main__':
    slicePass(sys.argv[1])
