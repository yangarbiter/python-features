import os, sys, json
from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
from utils import doForNonDirs
from multiprocessing import Pool

TIMEOUT=3

ANF_dataFolder = ""
ANF_outFolder = ""

def func(fileName):
    fullPath = os.path.join(ANF_dataFolder,fileName)
    if os.path.isdir(fullPath):
        return
    with open(fullPath, 'r') as logFile:
        with open(os.path.join(ANF_outFolder,fileName), 'w') as outFile:
            for line in logFile:
                dct = json.loads(line)
                if "PF_exitPipelineReason" in dct:
                    outFile.write(line)
                    continue
                code = dct['user_script']
                try:
                    x = run(["./.stack-work/install/x86_64-osx/lts-8.14/8.0.2/bin/make-anf-raw", code], stdout=PIPE, stderr=PIPE, encoding="utf-8", errors="strict", timeout=TIMEOUT, check=True)
                    dct['PF_anf_user_script'] = x.stdout
                except CalledProcessError as e:
                    dct['PF_exitPipelineReason'] = ("ANF error", e.stderr)
                # except TimeoutExpired:
                #     dct['PF_exitPipelineReason'] = ("ANF timeout", TIMEOUT)
                outFile.write(json.dumps(dct)+"\n")
        os.remove(fullPath)

def anfPass(dataFolder):
    global ANF_dataFolder
    global ANF_outFolder
    ANF_dataFolder = dataFolder
    ANF_outFolder = os.path.join(dataFolder, 'updated-with-anf')
    os.mkdir(ANF_outFolder)
    with Pool() as p:
        p.map(func, os.listdir(dataFolder))

if __name__ == '__main__':
    anfPass(sys.argv[1])
