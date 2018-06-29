from make_trace import BadInputException, type_and_slice
import sys, json, ast
from collections import defaultdict

def unANF(source, exceptionSpanANF, slice_spansANF, UD_1ANF):
    lines = source.split('\n')
    lineMap = {}
    for (i, line) in enumerate(lines):
        lineMap[i+1] = getSpan(line)

    def lineMapF(line):
        if line == None or line not in lineMap:
            raise BadInputException("Sourcemap fail 1")
        result = lineMap[line]
        if result == None:
            raise BadInputException("Sourcemap fail 2")
        return result

    exceptionSpan = lineMapF(exceptionSpanANF)
    slice_spans = list(map(lineMapF,slice_spansANF))
    # UD_1 = {lineMapF(k): list(map(lineMapF,v)) for (k,v) in UD_1ANF.items()}
    UD_1 = defaultdict(set)
    for (useLineNum, defLineNums) in UD_1ANF.items():
        useSpan = lineMap[useLineNum]
        if useSpan == None:
            continue
        for defLineNum in defLineNums:
            defSpan = lineMap[defLineNum]
            if defSpan == None:
                continue
            if contains(useSpan, defSpan):
                continue
            UD_1[useSpan].add(defSpan)

    return (exceptionSpan, slice_spans, dict(UD_1))

###############
## Working with spans
def convertSpan(span):
    span = span.strip().split("-")
    if len(span) == 1:
        span.append(span[0])
    [(a,b), (c,d)] = [ast.literal_eval(x) for x in span]
    return (a,b,c,d)

def getSpan(line):
    parts = line.split("#  interaction:")
    if len(parts) == 2:
        return convertSpan(parts[1])
    else:
        return None

def contains(superSpan, subSpan):
    (a,b,c,d) = superSpan
    (w,x,y,z) = subSpan
    return (a,b) <= (w,x) and (y,z) <= (c,d)
#################

def process_one(string):
    obj = json.loads(string)
    source = obj['PF_anf_user_script']
    ri = json.dumps(obj['raw_input']) if 'raw_input' in obj else '[]'

    (type_info, result) = type_and_slice(source, ri)
    exceptionSpanANF, slice_spansANF, exceptionMsg, UD_1ANF = result
    if exceptionSpanANF == None:
        exceptionSpan, slice_spans, UD_1 = exceptionSpanANF, slice_spansANF, None
    else:
        exceptionSpan, slice_spans, UD_1 = unANF(source, exceptionSpanANF, slice_spansANF, UD_1ANF)
    return (exceptionSpan, type_info, slice_spans, exceptionMsg, UD_1, UD_1ANF)

if __name__ == '__main__':
    print(process_one(sys.argv[1]))
    # with open(sys.argv[1], 'r') as f:
    #     lines = f.readlines()
    #     obj = {'PF_anf_user_script': '\n'.join(lines)}
    #     print(process_one(json.dumps(obj)))
