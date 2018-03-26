### Annotates each submission with an ANF'd version of the code
from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
from utils import doFunc

TIMEOUT=3
ANF_BIN="./.stack-work/install/x86_64-osx/lts-8.14/8.0.2/bin/make-anf-raw"
# ANF_BIN="./make-anf-raw"

def anfFunc(fileName, dataFolder, outFolder):
    doFunc(actualFunc, fileName, dataFolder, outFolder)

def actualFunc(line, dct, outFile):
    code = dct['user_script']
    try:
        x = run([ANF_BIN, code], stdout=PIPE, stderr=PIPE,
                encoding="utf-8", errors="strict", timeout=TIMEOUT, check=True)
        dct['PF_anf_user_script'] = x.stdout
    except CalledProcessError as e:
        dct['PF_exitPipelineReason'] = ("ANF error", e.stderr)
    # except TimeoutExpired:
    #     dct['PF_exitPipelineReason'] = ("ANF timeout", TIMEOUT)
