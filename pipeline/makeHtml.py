import json, sys, ast, os
from subprocess import run, PIPE
import functools

ML_DIR = 'foo'
ML_EXT = '.ml'
CSV_DIR = 'goodCsvs'
CSV_EXT = '.csv'

HTML_BEGIN = '''<head>
<link rel="stylesheet" type="text/css" href="visualization.css">
</head>
<body>
<div class="container">'''
HTML_END = '''Key:
<br/>
<div class="box blame">blame</div> / <div class="box">no blame</div>
<br/>
<div class="box change">change</div> / <div class="box">no change</div>
<br/>
<br/>
</div>
</body>
'''
#<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>
#<script src="foo.js"></script>
# $(".box").children().hover(function () {
#     $(this).parent().css("border-color", "grey");
#     $(this).css("border-color", "red");
# }, function () {
#     $(this).parent().css("border-color", "");
#     $(this).css("border-color", "");
# });

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
    if (beginOrder == 0 and endOrder == 0) or (beginOrder == -1 and overlapCheck2 == 1) or (beginOrder == 1 and overlapCheck1 == -1):
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
                    outBuffer += '<div class="%s" title="%f\n%s">' % (" ".join(classes),confidence,rules)
            outBuffer += char
            for fact in facts:
                (_, _, r2, c2) = fact[0]
                if row+1 == r2 and col+1 == c2:
                    outBuffer += '</div>'
        outBuffer += '<br/>\n'
    return outBuffer

if __name__ == '__main__':
    idx = sys.argv[1]
    progSection = True
    prog = ""
    with open(os.path.join(ML_DIR,idx+ML_EXT)) as inFile:
        for line in inFile:
            if line == "(* fix\n":
                break
            else:
                prog += line

    proc = run("python2 learning/decisionpath.py models/decision-tree-goodCsvs.pkl".split()+[os.path.join(CSV_DIR,idx+CSV_EXT)],
            stdout=PIPE, stderr=PIPE, encoding="utf-8", errors="strict")
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
    with open(sys.argv[2], 'w') as outFile:
        outFile.write(HTML_BEGIN + htmlBody + HTML_END)
