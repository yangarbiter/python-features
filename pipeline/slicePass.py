### Annotates each submission with whether it runs to completion without crash
from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
from utils import doFunc

TIMEOUT=2  # max time per program (sec)

def sliceFunc(fileName, dataFolder, outFolder):
    doFunc(actualFunc, fileName, dataFolder, outFolder)

def actualFunc(line, dct, outFile):
    try:
        # running as separate process for ease of TIMEOUT
        x = run(["python3", "make_trace/process_raw.py", line], stdout=PIPE, stderr=PIPE,
                encoding="utf-8", errors="strict", timeout=TIMEOUT, check=True)
        dct["PF_slicerOutput"] = x.stdout
    except CalledProcessError as e:
        dct['PF_exitPipelineReason'] = ("Slicer error", e.stderr)
    except TimeoutExpired:
        dct['PF_exitPipelineReason'] = ("Slicer timeout", TIMEOUT)

# Invocation example:
# docker build -t python-munge .
# docker run -v PF_DATA:/data python-munge
