import json, sys, ast, os
from subprocess import run, PIPE
import functools, html

ML_DIR = 'v4-partial/vectors'
ML_EXT = '.ml'
CSV_DIR = 'v4-partial/vectors/blah+context+slice+size'
CSV_EXT = '.csv'

HTML_BEGIN = '''<head>
<link rel="stylesheet" type="text/css" href="visualization.css">
</head>
<body>
<div class="container">'''
HTML_MAIN_END = '''<br/>
</div>
Key:
<br/>
<div class="box blame">blame</div> - <div class="box">no blame</div>
<br/>
<div class="box change">change</div> / <div class="box">no change</div>
<br/>
<br/>
<br/>
'''
HTML_FIX_BEGIN = '''<div class="container">'''
HTML_END = '''
<br/>
</div>
</body>
'''

# TODO: handle changes outside of slice

# http://portingguide.readthedocs.io/en/latest/comparisons.html
def cmp(x, y):
    """
    Replacement for built-in function cmp that was removed in Python 3

    Compare the two objects x and y and return an integer according to
    the outcome. The return value is negative if x < y, zero if x == y
    and strictly positive if x > y.
    """
    return (x > y) - (x < y)

def spanComparator(span1, span2):
    (a1, b1, c1, d1) = span1
    (a2, b2, c2, d2) = span2
    beginOrder = cmp((a1, b1), (a2, b2))
    endOrder = cmp((c1, d1), (c2, d2))
    overlapCheck1 = cmp((a1, b1), (c2, d2))
    overlapCheck2 = cmp((c1, d1), (a2, b2))
    if (beginOrder == 0 and endOrder == 0) or (beginOrder == -1 and overlapCheck2 == 1 and endOrder == -1) or (beginOrder == 1 and overlapCheck1 == -1 and endOrder == 1):
        print(span1, span2)
        print(beginOrder, endOrder, overlapCheck1, overlapCheck2)
        raise hell
    elif beginOrder != 0:
        return beginOrder
    else:
        return -endOrder

def factComparator(fact1, fact2):
    return spanComparator(fact1[0], fact2[0])

def visualize(code, facts):
    code = code.split('\n')
    outBuffer = ""
    z = 0
    for (row,line) in enumerate(code):
        for (col,char) in enumerate(line):
            for fact in facts:
                (span, confidence, didChange, rules) = fact
                (r1, c1, _, _) = span
                if row+1 == r1 and col+1 == c1:
                    classes = ["box"]
                    if didChange == 1.0:
                        classes.append("change")
                    if confidence >= 0.5:
                        classes.append("blame")
                    color = int((1-confidence)*120)
                    z += 1
                    if z > 9:
                        print("warning: can't handle z > 9")
                    classes.append("b%d" % z)
                    tooltipText = "Confidence: %f<br/>%s" % (confidence,'<br/>'.join(rules.split('\n')))
                    outBuffer += '<div class="%s" style="border-color:hsl(%d, 100%%, 50%%)"><span class="span%d">%s</span>' % (" ".join(classes),color,z,tooltipText)
            outBuffer += html.escape(char, quote=False).replace(' ', '&nbsp')
            for fact in facts:
                (_, _, r2, c2) = fact[0]
                if row+1 == r2 and col+1 == c2:
                    z -= 1
                    outBuffer += '</div>'
        outBuffer += '<br/>\n'
    return outBuffer

def runNames(inName, outName):
    progSection = True
    prog = ""
    fix = ""
    with open(os.path.join(ML_DIR,inName+ML_EXT)) as inFile:
        for line in inFile:
            if line == "(* fix\n":
                progSection = False
                continue
            if line == "*)\n":
                break
            if progSection:
                prog += line
            else:
                fix += line

    proc = run("python3 pipeline/decisionpath.py ../blame/data_splits/v4_encoder.pkl ../blame/models/v4_DecisionTree_p@1.pkl".split()+[os.path.join(CSV_DIR, inName+CSV_EXT)],
            stdout=PIPE, stderr=PIPE, encoding="utf-8", errors="strict")
    if proc.returncode != 0:
        print("decisionpath failed!")
        raise hell
    facts = []
    span = None
    confidence = None
    didChange = None
    rules = ""
    for line in proc.stdout.split('\n'):
        if line == "----------------------------------":
            facts.append((ast.literal_eval(span), float(confidence), float(didChange), rules))
            span = None
            confidence = None
            didChange = None
            rules = ""
        elif span == None:
            span = line
        elif confidence == None:
            confidence = line
        elif didChange == None:
            didChange = line
        else:
            rules += line+'\n'
    facts.sort(key=functools.cmp_to_key(factComparator))

    htmlBody = visualize(prog, facts)
    with open(outName, 'w') as outFile:
        htmlFix = html.escape(fix, quote=False).replace(' ', '&nbsp').replace('\n', '<br/>\n')
        outFile.write(HTML_BEGIN + htmlBody + HTML_MAIN_END + HTML_FIX_BEGIN + htmlFix + HTML_END)

if __name__ == '__main__':
    weWin = ['129830', '278772', '130221', '125360', '127768', '151698', '128115', '100291', '300952', '116942', '281819']
    for i in weWin[:1]:
        runNames(i, "temp/"+i+".html")
    # runNames(sys.argv[1], "temp/"+sys.argv[1]+".html")#sys.argv[2])
