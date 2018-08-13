import ast
import json
import Levenshtein as lev
from string_id import StringIdMaker

ider = StringIdMaker()

class Node():
    def __init__(self, a):
        self.children = list(map(Node, ast.iter_child_nodes(a)))
        self.name = ider.make_id(type(a))
        
def node_transform(a):
    return Node(a)

def build_node_strings(out_list, node):
    # Mark the beginning of this node by an id for that node type
    out_list.append(chr(node.name))

    # Add the contents of this node
    for child in node.children:
        build_node_strings(out_list, child)

    # Token marking end of node
    out_list.append(chr(ider.make_id('end')))

# Takes 1 element arrays containing the strings
def metric(a, b):
    return lev.distance(a[0],b[0])

def transform_python(a):
    node = node_transform(a)
    string_list = []
    build_node_strings(string_list, node)
    return ''.join(string_list)

ocaml_node_list = ['int', 'double', 'bool', 'char', 'string',
                   'let', 'letrec', 'eval', 'type', 'exception',
                   'var', 'fun', 'app', 'bop', 'uop', 'lit', 'ite', 'seq', 'case',
                   'tuple', 'con', 'record', 'getField', 'setField', 'array',
                   'list', 'try',
                   'interval', 'const', 'wild', 'or', 'as', 'type']

ocaml_node_set = set(ocaml_node_list)

def build_ocaml_strings(out_list, j):
    if isinstance(j, list):
        for element in j:
            build_ocaml_strings(out_list, element)
    elif isinstance(j, dict):
        # Sort them so that children in dicts are always output in the same order
        keys = sorted(j.keys())

        for k in keys:
            if k in ocaml_node_set:
                # This is part of the ast where multiple node types would be
                # valid. Add this to the output to preserve this info
                out_list.append(chr(ider.make_id(k)))
                build_ocaml_strings(out_list, j[k])

                # End of node marker
                out_list.append(chr(ider.make_id('end')))
            else:
                build_ocaml_strings(out_list, j[k])
                

def transform_ocaml(j):
    string_list = []
    build_ocaml_strings(string_list, j)
    return ''.join(string_list)
