# From the original raw data, extracts py3 submissions, annotates them
# according to whether they parse, filters them by event type, and sorts them
# by server time
import json, ast, os, sys
from collections import defaultdict
from utils import doForNonDirs

DATA_FOLDER = sys.argv[1]
LANG = 'py3'
EVENT_TYPE = 'web_exec'
OUT_FOLDER = sys.argv[2]+'/'+LANG+'-'+EVENT_TYPE

os.mkdir(OUT_FOLDER)

def checkList(dctlist):
    '''Returns True if at least two dictionaries in the list have "PF_parses" as "yes"'''
    parses = 0
    for dct in dctlist:
        if dct["PF_parses"] == "yes":
            parses += 1
            if parses > 1:
                return True
    return False

def handleFile(_fileName, logFile):
    for line in logFile:
        dct = json.loads(line)
        if 'lang' not in dct or dct['lang'] != LANG or 'event_type' not in dct or dct['event_type'] != EVENT_TYPE or 'session_uuid' not in dct:
            continue
        try:
            # py_compile.compile('temp.py', doraise=True) #TODO more accurate (rejects "Return"), but can timeout (e.g. on 'print(8 ** 1000000000000000)')
            ast.parse(dct['user_script'])
            dct['PF_parses'] = "yes"
        except SyntaxError:
            dct['PF_parses'] = "SyntaxError"
            dct['PF_exitPipelineReason'] = ("Does not parse", "Syntax Error")
        except ValueError:
            dct['PF_parses'] = "ValueError"
            dct['PF_exitPipelineReason'] = ("Does not parse", "Value Error")
        uuid = dct['session_uuid']
        results[uuid].append(dct)

results = defaultdict(list)
doForNonDirs(DATA_FOLDER, handleFile)
for uuid in results.keys():
    if checkList(results[uuid]):
        results[uuid].sort(key=lambda x: x['serverTimeUTC'])
        with open(os.path.join(OUT_FOLDER,uuid), 'w') as outFile:
            for dct in results[uuid]:
                outFile.write(json.dumps(dct)+'\n')
