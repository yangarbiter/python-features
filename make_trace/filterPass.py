# From the original raw data, extracts py3 submissions, annotates them
# according to whether they parse, filters them by event type, and sorts them
# by server time
import json, ast, os, sys, collections
from utils import doForNonDirs

LANG = 'py3'
EVENT_TYPE = 'web_exec'

def checkList(dctlist):
    '''Returns True if at least two dictionaries in the list have "PF_parses" as True'''
    parses = 0
    for dct in dctlist:
        if dct["PF_parses"]:
            parses += 1
            if parses > 1:
                return True
    return False

def handleFile(logFile, results):
    for line in logFile:
        dct = json.loads(line)
        if 'lang' not in dct or dct['lang'] != LANG or 'event_type' not in dct or dct['event_type'] != EVENT_TYPE or 'session_uuid' not in dct:
            continue
        try:
            # py_compile.compile('temp.py', doraise=True) #TODO more accurate (rejects "Return"), but can timeout (e.g. on 'print(8 ** 1000000000000000)')
            ast.parse(dct['user_script'])
            dct['PF_parses'] = True
        except SyntaxError:
            dct['PF_parses'] = False
            dct['PF_exitPipelineReason'] = ("Does not parse", "Syntax Error")
        except ValueError:
            dct['PF_parses'] = False
            dct['PF_exitPipelineReason'] = ("Does not parse", "Value Error")
        results[dct['session_uuid']].append(dct)

def filterPass(dataFolder, outFolder):
    outFolder = outFolder+'/'+LANG+'-'+EVENT_TYPE
    os.mkdir(outFolder)
    results = collections.defaultdict(list)
    doForNonDirs(dataFolder, lambda fileName, logFile: handleFile(logFile, results))
    for uuid in results.keys():
        if checkList(results[uuid]):
            results[uuid].sort(key=lambda x: x['serverTimeUTC'])
            with open(os.path.join(outFolder,uuid), 'w') as outFile:
                for dct in results[uuid]:
                    outFile.write(json.dumps(dct)+'\n')

if __name__ == '__main__':
    filterPass(sys.argv[1], sys.argv[2])
