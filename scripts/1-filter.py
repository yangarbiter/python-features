# From the original raw data, extracts py3 submissions, annotates them
# according to whether they parse, filters them by event type, and sorts them
# by server time
import json, ast, os, sys
from collections import defaultdict

DATA_FOLDER = 'data/raw/'
LANG = 'py3'
EVENT_TYPE = 'web_exec'
OUT_FOLDER = 'data/filtered/'+LANG+'-'+EVENT_TYPE+'/'

def checkList(dctlist):
    '''Returns True if at least two dictionaries in the list have "__parses" as "yes"'''
    parses = 0
    for dct in dctlist:
        if dct["__parses"] == "yes":
            parses += 1
        if parses > 1:
            return True
    return False

results = defaultdict(list)
for fileName in os.listdir(DATA_FOLDER):
# for fileName in ['access_pythontutor_php.log.2017-06-03.gz.jsonl']:
    print(fileName)
    sys.stdout.flush()
    with open(DATA_FOLDER+fileName, 'r') as logFile:
        for line in logFile:
            dct = json.loads(line)
            if 'lang' not in dct or dct['lang'] != LANG or 'event_type' not in dct or dct['event_type'] != EVENT_TYPE or 'session_uuid' not in dct:
                continue
            try:
                # py_compile.compile('temp.py', doraise=True) #TODO more accurate (rejects "Return"), but can timeout (e.g. on 'print(8 ** 1000000000000000)')
                ast.parse(dct['user_script'])
                dct['__parses'] = "yes"
            except SyntaxError:
                dct['__parses'] = "SyntaxError"
            except ValueError:
                dct['__parses'] = "ValueError"
            uuid = dct['session_uuid']
            results[uuid].append(dct)
os.mkdir(OUT_FOLDER)
for uuid in results.keys():
    if checkList(results[uuid]):
        results[uuid].sort(key=lambda x: x['serverTimeUTC'])
        with open(OUT_FOLDER+uuid, 'w') as outFile:
            for dct in results[uuid]:
                outFile.write(json.dumps(dct)+'\n')
