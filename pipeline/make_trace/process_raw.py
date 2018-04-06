import make_trace
import sys, json

def process_one(string):
    obj = json.loads(string)
    source = obj['PF_anf_user_script']
    ri = json.dumps(obj['raw_input']) if 'raw_input' in obj else '[]'

    result = make_trace.slice(source, ri)
    type_info = make_trace.extract_type_info(source, ri)
    if result == None:
        return (None, type_info, [])
    else:
        exceptionSpan, slice_spans = make_trace.slice(source, ri)
        return (exceptionSpan, type_info, slice_spans)

if __name__ == '__main__':
    print(process_one(sys.argv[1]))
