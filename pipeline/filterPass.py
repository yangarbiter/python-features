# From the original raw data, extracts py3 submissions, annotates them
# according to whether they parse, filters them by event type
import json, ast, os, sys, collections
from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
import multiprocessing.pool
from utils import makePass

LANG = 'py3'
EVENT_TYPE = 'web_exec'
def formatFilter(dct):
    if 'lang' not in dct or dct['lang'] != LANG or 'event_type' not in dct or dct['event_type'] != EVENT_TYPE or 'session_uuid' not in dct:
        return ("Incorect language/event_type/session_uuid", None)

def parseFilter(dct):
    try:
        # py_compile.compile('temp.py', doraise=True) #TODO more accurate (rejects "Return"), but can timeout (e.g. on 'print(8 ** 1000000000000000)')
        x = ast.parse(dct['user_script'])
        dct['PF_parses'] = True
        if x.body == []:
            return ("Empty submission", None)
    except SyntaxError:
        dct['PF_parses'] = False
        return ("Does not parse", "Syntax Error")
    except ValueError:
        dct['PF_parses'] = False
        return ("Does not parse", "Value Error")

### Annotates each submission with an ANF'd version of the code
ANF_TIMEOUT=3
ANF_BIN="../.stack-work/install/x86_64-osx/lts-8.14/8.0.2/bin/make-anf-raw"
def anfPass(dct):
    code = dct['user_script']
    try:
        x = run([ANF_BIN, code], stdout=PIPE, stderr=PIPE,
                encoding="utf-8", errors="strict", timeout=ANF_TIMEOUT, check=True)
        dct['PF_anf_user_script'] = x.stdout
    except CalledProcessError as e:
        return ("ANF error", e.stderr)

if __name__ == '__main__':
    passes = [makePass(f,i) for (i,f) in enumerate([formatFilter, parseFilter, anfPass])]
    def doPasses(line):
        dct = json.loads(line)
        for action in passes:
            dct = action(dct)
        return dct
    pool = multiprocessing.pool.Pool()
    for result in pool.imap_unordered(doPasses, sys.stdin, 100):
        print(json.dumps(result))

#TODO:
# results[uuid].sort(key=lambda x: x['serverTimeUTC'])
