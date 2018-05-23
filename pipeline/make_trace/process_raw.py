import make_trace
import sys, json

def process_one(string):
    obj = json.loads(string)
    source = obj['PF_anf_user_script']
    ri = json.dumps(obj['raw_input']) if 'raw_input' in obj else '[]'

    (type_info, result) = make_trace.type_and_slice(source, ri)
    if result == None:
        return (None, type_info, [])
    else:
        exceptionSpan, slice_spans, exceptionMsg, UD_1 = result
        return (exceptionSpan, type_info, slice_spans, exceptionMsg)

if __name__ == '__main__':
    print(process_one(sys.argv[1]))
    # with open(sys.argv[1], 'r') as f:
    #     print(process_one(f.readline()))
