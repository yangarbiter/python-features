import subprocess, os, sys, json
from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
from utils import doForNonDirs
from multiprocessing import Pool

DATA_FOLDER=sys.argv[1]
OUT_FOLDER=os.path.join(DATA_FOLDER, 'updated-with-anf')
TIMEOUT=3

os.mkdir(OUT_FOLDER)

def func(fileName):
    fullPath = os.path.join(DATA_FOLDER,fileName)
    if os.path.isdir(fullPath):
        return
    with open(fullPath, 'r') as logFile:
        with open(os.path.join(OUT_FOLDER,fileName), 'w') as outFile:
            for line in logFile:
                dct = json.loads(line)
                if "PF_exitPipelineReason" in dct:
                    outFile.write(line)
                    continue
                code = dct['user_script']
                try:
                    x = run(["stack", "exec", "--", "make-anf", code], stdout=PIPE, stderr=PIPE, encoding="utf-8", errors="strict", timeout=TIMEOUT, check=True)
                    dct['PF_anf_user_script'] = x.stdout
                except CalledProcessError as e:
                    dct['PF_exitPipelineReason'] = ("ANF error", e.stderr)
                except TimeoutExpired:
                    dct['PF_exitPipelineReason'] = ("ANF timeout", TIMEOUT)
                outFile.write(json.dumps(dct))
        os.remove(fullPath)

with Pool() as p:
    print(p.map(func, os.listdir(DATA_FOLDER)))

# step "2" of pipeline:
# subprocess.check_output(["docker", "run", "-v", "/Users/benjamin/LessTemporaryDownloads/may-to-july-2017-server-logs/data:/app/data", "python-munge"])
