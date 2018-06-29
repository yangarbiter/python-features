from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
import sys, os, json
import multiprocessing.pool
from utils import makePass
from optparse import OptionParser

SLICE_TIMEOUT=10  # max time per program (sec)
def slicePass(dct):
    try:
        # running as separate process for ease of TIMEOUT
        x = run(["python3", "make_trace/process_raw.py", json.dumps(dct)], stdout=PIPE, stderr=PIPE,
                encoding="utf-8", errors="strict", timeout=SLICE_TIMEOUT, check=True)
        dct["PF_slicerOutput"] = x.stdout
    except CalledProcessError as e:
        msg = e.stderr
        if "BadInputException:" in msg:
            err = msg.split("BadInputException: ")[-1].strip()
            return (err, None)
        else:
            return ("Slicer error", e.stderr)
    except TimeoutExpired:
        return ("Slicer timeout", SLICE_TIMEOUT)
    except Exception as e:
        return ("Other slicePass error", str(e))

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option("-r", "--rerun", action="store_true", default=False, dest="rerun")
    (options, args) = parser.parse_args()

    passes = [makePass(slicePass, 3, options.rerun)]
    def doPasses(line):
        dct = json.loads(line)
        for action in passes:
            dct = action(dct)
        return dct
    pool = multiprocessing.pool.Pool()
    for result in pool.imap_unordered(doPasses, sys.stdin, 100):
        print(json.dumps(result))
