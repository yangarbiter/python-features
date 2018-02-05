import make_trace
import sys
import traceback

DEBUG = True

def process_one(source):
    # ri = json.dumps(obj['raw_input']) if 'raw_input' in obj else '[]'
    ri = '[]'
    slice_lines, slice_p = make_trace.slice(source, ri, debug=True)

    if slice_lines:
        print(str(list(slice_lines)))
    else:
        print("No exception")

source = sys.argv[1]
try:
    process_one(source)
except Exception as e:
    if DEBUG:
        traceback.print_exc(None, sys.stdout)
    else:
        print(e)
