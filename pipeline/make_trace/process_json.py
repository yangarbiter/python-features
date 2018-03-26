import make_trace
import ast, sys, json
import traceback

DEBUG = True
decoder = json.JSONDecoder()

def process_one(outfile, string):
    obj = decoder.decode(string)
    source = obj['user_script']
    ri = json.dumps(obj['raw_input']) if 'raw_input' in obj else '[]'

    last_line = len(source.splitlines())
    slice_lines, slice_p, slice_spans = make_trace.slice(source, ri, debug=True)
    type_info = make_trace.extract_type_info(source, ri)

    if slice_lines:
        print('Original code:')
        print(source)

        print('Lines to keep: ' + str(slice_lines))
        print('Line proportion removed: ' + str(slice_p))
        print('Expression spans to keep: ' + str(slice_spans))

        print('Types:')
        print(type_info)

        obj['exception_slice'] = list(slice_lines)
        obj['id_to_type'] = type_info
        obj['slice_expr_spans'] = slice_spans
    else:
        print("No exception")

    json.dump(obj, outfile)

with open(sys.argv[1]) as infile:
    with open(sys.argv[1] + '.sliced', 'wt') as outfile:

        for i, line in enumerate(infile):
            print('Interaction ' + str(i))
            try:
                process_one(outfile, line)
            except Exception as e:
                if DEBUG:
                    traceback.print_exc(None, sys.stdout)
                else:
                    print(e)
            print('')
