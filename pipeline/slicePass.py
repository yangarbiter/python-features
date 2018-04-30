### Annotates each submission with whether it runs to completion without crash
from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
from utils import doFunc, doPass
import sys, os

TIMEOUT=2  # max time per program (sec)

def sliceFunc(fileName, dataFolder, outFolder, ignoredFiles):
    doFunc(actualFunc, fileName, dataFolder, outFolder, ignoredFiles)

def actualFunc(line, dct, outFile):
    try:
        # running as separate process for ease of TIMEOUT
        x = run(["python3", "make_trace/process_raw.py", line], stdout=PIPE, stderr=PIPE,
                encoding="utf-8", errors="strict", timeout=TIMEOUT, check=True)
        dct["PF_slicerOutput"] = x.stdout
    except CalledProcessError as e:
        msg = e.stderr
        if "BadInputException: Sourcemap fail" in msg:
            dct['PF_exitPipelineReason'] = ("ANF->slicer sourcemap", None)
        elif "BadInputException: Insufficient raw input" in msg:
            dct['PF_exitPipelineReason'] = ("Insufficient raw input", None)
        elif "BadInputException: Runs more than 1000 steps" in msg:
            dct['PF_exitPipelineReason'] = ("Runs more than 1000 steps", None)
        else:
            dct['PF_exitPipelineReason'] = ("Slicer error", e.stderr)
    except TimeoutExpired:
        dct['PF_exitPipelineReason'] = ("Slicer timeout", TIMEOUT)

# Invocation example:
# docker build -t python-pipeline .
# docker run -v PF_DATA:/data python-pipeline

if __name__ == '__main__':
    doPass(sliceFunc, sys.argv[1], 'sliced')