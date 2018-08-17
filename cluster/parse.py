import json
import glob

decoder = json.JSONDecoder()

def read_files_from_glob(g):
    paths = glob.glob(g)
    sources = []
    for path in paths:
        with open(path) as f:
            sources.append(f.read())

    return sources

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
