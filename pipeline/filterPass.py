# From the original raw data, extracts py3 submissions, annotates them
# according to whether they parse, filters them by event type, and sorts them
# by server time
import json, ast, os, sys, collections

LANG = 'py3'
EVENT_TYPE = 'web_exec'

def checkList(dctlist):
    '''Returns True if at least two dictionaries in the list have not been filtered out'''
    ok = 0
    for dct in dctlist:
        if "PF_exitPipelineReason" not in dct:
            ok += 1
    return (ok > 1)

def handleLine(dct, results):
    if 'lang' not in dct or dct['lang'] != LANG or 'event_type' not in dct or dct['event_type'] != EVENT_TYPE or 'session_uuid' not in dct:
        return
    try:
        # py_compile.compile('temp.py', doraise=True) #TODO more accurate (rejects "Return"), but can timeout (e.g. on 'print(8 ** 1000000000000000)')
        x = ast.parse(dct['user_script'])
        dct['PF_parses'] = True
        if x.body == []:
            dct['PF_exitPipelineReason'] = ("Empty submission", None)
    except SyntaxError:
        dct['PF_parses'] = False
        dct['PF_exitPipelineReason'] = ("Does not parse", "Syntax Error")
    except ValueError:
        dct['PF_parses'] = False
        dct['PF_exitPipelineReason'] = ("Does not parse", "Value Error")
    results[dct['session_uuid']].append(dct)

def filterPass(outFolder):
    outFolder = outFolder+'/'+LANG+'-'+EVENT_TYPE
    os.makedirs(outFolder)
    results = collections.defaultdict(list)
    for line in sys.stdin:
        try:
            dct = json.loads(line)
            handleLine(dct, results)
        except json.decoder.JSONDecodeError:
            print("WEIRD JSON DECODE IN LINE: %s" % line)
    for uuid in results.keys():
        if checkList(results[uuid]):
            results[uuid].sort(key=lambda x: x['serverTimeUTC'])
            with open(os.path.join(outFolder,uuid), 'w') as outFile:
                for dct in results[uuid]:
                    outFile.write(json.dumps(dct)+'\n')

if __name__ == '__main__':
    filterPass(sys.argv[1])
