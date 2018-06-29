from subprocess import run, PIPE, CalledProcessError, TimeoutExpired
from process_raw import process_one
from collections import defaultdict
import json
import ast

ANF_BIN="../../.stack-work/install/x86_64-osx/lts-8.14/8.0.2/bin/make-anf-raw"
ANF_TIMEOUT=3

def contains(superSpan, subSpan):
    (a,b,c,d) = superSpan
    (w,x,y,z) = subSpan
    return (a,b) <= (w,x) and (y,z) <= (c,d)

def getSpan(line):
    parts = line.split("#  interaction:")
    if len(parts) == 2:
        return convertSpan(parts[1])
    else:
        return None

def convertSpan(span):
    span = span.strip().split("-")
    if len(span) == 1:
        span.append(span[0])
    [(a,b), (c,d)] = [ast.literal_eval(x) for x in span]
    return (a,b,c,d)

with open('../temp.py', 'r') as inFile:
    code = ''.join(inFile.readlines())
    print("----------------------------------------")
    print("Original code:")
    print(code)

    x = run([ANF_BIN, code], stdout=PIPE, stderr=PIPE,
        encoding="utf-8", errors="strict", timeout=ANF_TIMEOUT, check=True)
    anfCode = x.stdout
    print("----------------------------------------")
    print("ANF code:")
    print(anfCode)
    # anfCode = code

    result = process_one(json.dumps({"PF_anf_user_script": anfCode}))
    print("----------------------------------------")
    print("Slicer result:")
    print(result)

    UD_1 = result[4]
    print("----------------------------------------")
    print("ud:")
    print(UD_1)
