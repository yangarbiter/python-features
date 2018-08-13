import json
import glob

decoder = json.JSONDecoder()

def read_from_glob(fun, g):
    paths = glob.glob(g)
    results = []
    for path in paths:
        with open(path) as f:
            try:
                results.append(fun(f))
            except Exception as e:
                print(e)
    return results

def read_python(f):
    last = next(f, "")
    obj = decoder.decode(last)
    return obj['fix']

def python_sources_from_glob(g):
    return read_from_glob(read_python, g)

def python_sources_from_glob(g):
    paths = glob.glob(g)
    sources = []
    for path in paths:
        with open(path) as f:
            last = next(f, "")
            try:
                obj = decoder.decode(last)
                source = obj['fix']
                sources.append(source)
            except Exception as e:
                print(e)
    return sources
