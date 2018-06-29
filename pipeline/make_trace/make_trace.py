import sys, os, ast, re
import pg_logger
from collections import defaultdict
from copy import deepcopy
from queue import Queue

class BadInputException(Exception):
    pass

class VarEnvironment():
    def __init__(self, execution_point):
        if execution_point['event'] == 'instruction_limit_reached':
            raise BadInputException("Runs more than 1000 steps")
        if execution_point['event'] == 'exception' and execution_point['exception_msg'][:9] == 'NameError':
            raise BadInputException("Input has NameError")
        if execution_point['event'] == 'exception' and execution_point['exception_msg'][:17] == 'UnboundLocalError':
            raise BadInputException("Input has UnboundLocalError")
        if execution_point['event'] == 'uncaught_exception' and execution_point['exception_msg'][:38] == "SyntaxError: 'return' outside function":
            raise BadInputException("Input has return outside function")
        self.heap = execution_point['heap']
        self.globals = execution_point['globals']
        self.masked = set()
        if len(execution_point['stack_to_render']) > 0:
            frame = execution_point['stack_to_render'][-1]
            self.locals = frame['encoded_locals']
            self.frame_hash = frame['unique_hash']
        else:
            self.locals = {}

    def get_var(self, name):
        if name in self.masked:
            return 'undefined' + name
        
        if name in self.locals:
            return self.frame_hash + ':' + name
        elif name in self.globals:
            return 'global:' + name
        else:
            return 'undefined:' + name

    def mask_var(self, name):
        self.masked.add(name)

    def get_ref(self, name):
        if name in self.masked:
            return None
        
        if name in self.locals:
            [tag, ref] = self.locals[name]
            assert(tag == 'REF') # Ref primitives
            return ref
        elif name in self.globals:
            [tag, ref] = self.globals[name]
            assert(tag == 'REF') # Ref primitives
            return ref
        else:
            return None

    def get_val(self, name):
        ref = self.get_ref(name)
        if ref:
            return self.heap[ref]
        else:
            return None

    def vars(self):
        my_vars = self.heap.copy()

        for name in self.globals:
            if name not in self.masked:
                my_vars['global:' + name] = self.globals[name]

        for name in self.locals:
            if name not in self.masked:
                my_vars[self.frame_hash + ':' + name] = self.locals[name]

        return my_vars

    # Gets changes made in the second
    # Returns set of locations
    def diff(self, other):
        my_vars = self.vars()
        their_vars = other.vars()

        # New variables
        changes = their_vars.keys() - my_vars.keys()

        # Old variables that changed
        for loc in my_vars:
            if loc in their_vars and my_vars[loc] != their_vars[loc]:
                changes.add(loc)

        return changes

# Attempts to find an attribute for a value and identifier
# Returns a heap ref and a value
def find_attribute(var_env, val, identifier):
    if val[0] != 'INSTANCE':
        return None, None

    attr_pairs = val[2:]
    for ref1, ref2 in attr_pairs:
        assert(ref1[0] == 'REF')
        assert(ref2[0] == 'REF')
        key = var_env.heap[ref1[1]]
        assert(key[0] == 'HEAP_PRIMITIVE')
        assert(key[1] == 'str')
        if key[2] == identifier:
            value = var_env.heap[ref2[1]]
            return ref2[1], value

    return None, None

def is_heap_int(val):
    return val[0] == 'HEAP_PRIMITIVE' and val[1] == 'int'

def resolve_index(container_len, index):
    if index >= 0:
        return index
    else:
        return container_len + index

# Attempts to find a value by subscripting a value
# Returns a heap ref and a value
# Can return None for ref if the value is a new object
def find_subscript(var_env, container, index, upper_index = None):
    if container[0] ==  'LIST':
        if is_heap_int(index) and len(container) > index[2] + 1:
            actual_index = resolve_index(len(container) - 1, index[2])

            if upper_index and is_heap_int(upper_index):
                actual_upper_index = resolve_index(len(container) - 1, upper_index[2])
                if len(container) > max(actual_index + 1, actual_upper_index) and min(actual_index, actual_upper_index - 1) >= 0:
                    value = ['LIST']
                    for ref in container[actual_index + 1 : actual_upper_index + 1]:
                        assert(ref[0] == 'REF')
                        value.append(ref)
                    return None, value
            elif not upper_index:
                if len(container) > actual_index + 1 and actual_index >= 0:
                    ref = container[actual_index + 1]
                    assert(ref[0] == 'REF')
                    value = var_env.heap[ref[1]]
                    return ref[1], value

    elif container[0] == 'TUPLE':
        if index[0] == 'HEAP_PRIMITIVE' and index[1] == 'int' and len(container) > index[2] + 1:
            ref = container[index[2] + 1]
            assert(ref[0] == 'REF')
            value = var_env.heap[ref[1]]
            return ref[1], value
    elif container[0] == 'DICT':
        ass_list = container[1:0]
        for pair in ass_list:
            key_ref = pair[0]
            assert(key_ref[0] == 'REF')
            key = var_env.heap[key_ref[1]]
            if key == index:
                value_ref = pair[1]
                assert(value_ref[0] == 'REF')
                value = var_env.heap[value_ref[1]]
                return value_ref, value

    return None, None


# Attempts to find heap locations for the expression
# Returns a set of heap refs, plus a value for the overall expression
def find_refs(var_env, expr):
    if isinstance(expr, ast.Name):
        name_ref = var_env.get_ref(expr.id)
        return set([name_ref]), var_env.heap[name_ref]
    elif isinstance(expr, ast.Attribute):
        instance_refs, instance = find_refs(var_env, expr.value)

        if not instance:
            return instance_refs, None

        attr_ref, attr_value = find_attribute(var_env, instance, expr.attr)
        if attr_ref:
            instance_refs.add(attr_ref)
            return instance_refs, attr_value
        else:
            return instance_refs, None
    elif isinstance(expr, ast.Subscript):
        s = expr.slice

        container_refs, container = find_refs(var_env, expr.value)

        if not container:
            return container_refs, None

        if isinstance(s, ast.Index):
            index_refs, index = find_refs(var_env, s.value)
            if not index:
                return container_refs | index_refs, None

            sub_ref, sub_value = find_subscript(var_env, container, index)
        elif isinstance(s, ast.Slice):
            upper_refs, upper = find_refs(var_env, s.upper)
            lower_refs, lower = find_refs(var_env, s.lower)
            if not upper or not lower:
                return container_refs | upper_refs | lower_refs, None

            sub_ref, sub_value = find_subscript(var_env, container, lower, upper)
        else:
            raise ValueError("TODO: Support extended slicing")

        sub_refs = set()
        if sub_ref:
            sub_refs.add(sub_ref)

        return container_refs | index_refs | sub_refs, sub_value
    else:
        # raise 'Unsupported find_refs argument'
        return set(), None

ignored_events = set(['raw_input'])
def trace(source, ri):
    def finalizer(input_code, output_trace):
        filtered_trace = [ep for ep in output_trace if ep['event'] not in ignored_events]
        return filtered_trace

    return pg_logger.exec_script_str_local(source,
                                           ri,
                                           True,
                                           True,
                                           finalizer)

class LineMapVisitor(ast.NodeVisitor):
    def __init__(self):
        self.the_map = {}

    def visit(self, node):
        if isinstance(node, (ast.stmt, ast.excepthandler)):
            self.the_map[node.lineno] = node
        self.generic_visit(node)

"""
Currently only handles the case where statements are on separate lines.

TODO: Transform the original source so that statements are always on separate
lines.
"""
def make_line_maps(source):
    astree = ast.parse(source)
    map_visitor = LineMapVisitor()
    map_visitor.visit(astree)

    control_visitor = ControlVisitor()
    control_visitor.visit(astree)

    return map_visitor.the_map, control_visitor.line_to_controller, control_visitor.break_lines, control_visitor.handler_lines

class NameVisitor(ast.NodeVisitor):
    def __init__(self):
        self.names = set()

    def visit_Name(self, node):
        self.names.add(node.id)

class UseVisitor(ast.NodeVisitor):
    def __init__(self, exec_point):
        self.exec_point = exec_point
        self.env = VarEnvironment(exec_point)
        self.use_set = set()

    def die(self, node):
        raise ValueError('Unsupported node: ' + str(type(node)))

    def nothing(self, node):
        pass

    def handle_assignment_target(self, node):
        if isinstance(node, ast.Attribute):
            self.visit(node.value)
        elif isinstance(node, ast.Subscript):
            self.visit(node.value)
            self.visit(node.slice)
        elif isinstance(node, ast.List):
            self.visit(node)
        elif isinstance(node, ast.Tuple):
            self.visit(node)
        elif isinstance(node, ast.Name):
            pass
        else:
            raise ValueError('Unsupported assign target: ' + str(type(node)))

    # Exprs
    # BoolOp
    # BinOp
    # UnaryOp
    visit_Lambda = die
    # IfExp
    # Dict
    # Set
    def visit_ListComp(self, node):
        # Modify the env for some visits, so save the old one
        old_env = self.env
        self.env = deepcopy(old_env)

        for comp in node.generators:
            # Range expression
            self.visit(comp.iter)

            # Get the bindings made by the comprehension and mask them for the
            # rest of the expression, since they are internal
            name_visitor = NameVisitor()
            name_visitor.visit(comp.target)
            for name in name_visitor.names:
                self.env.mask_var(name)

            # Get use info from conditions
            for expr in comp.ifs:
                self.visit(expr)

        self.env = old_env
            

            
    
    visit_SetComp = die
    visit_DictComp = die
    visit_GeneratorExp = die
    visit_Await = die
    visit_Yield = die
    visit_YieldFrom = die
    # Compare
    # Call
    # Num
    # Str
    # FormattedValue
    # JoinedStr
    # Bytes
    # NameConstant
    visit_Ellipsis = die
    # Constant

    def visit_Attribute(self, node):
        refs, _ = find_refs(self.env, node)
        self.use_set |= refs

    visit_Subscript = visit_Attribute

    visit_Starred = die

    def visit_Name(self, node):
        self.use_set.add(self.env.get_var(node.id))
        self.use_set.add(self.env.get_ref(node.id))

    # List
    # Tuple

    # Stmts

    # TODO: Is this correct? Need to account for captured vars?
    visit_FunctionDef = nothing

    visit_AsyncFunctionDef = die
    visit_ClassDef = visit_FunctionDef # TODO: account for class-specific stuff
    # Return

    visit_Delete = die

    def visit_Assign(self, stmt):
        self.visit(stmt.value)
        for target in stmt.targets:
            self.handle_assignment_target(target)

    def visit_AugAssign(self, stmt):
        self.visit(stmt.target)
        self.visit(stmt.value)

    visit_AnnAssign = visit_Assign

    def visit_For(self, stmt):
        self.visit(stmt.iter)
        self.handle_assignment_target(stmt.target)

    visit_AsyncFor = die

    def visit_IfLike(self, stmt):
        self.visit(stmt.test)

    visit_While = visit_IfLike
    visit_If = visit_IfLike

    visit_With = die
    visit_AsyncWith = die
    # Raise
    
    # Try(self, stmt):
    
    # Assert
    visit_Import = nothing
    visit_ImportFrom = nothing
    visit_Global = nothing
    visit_Nonlocal = nothing
    # Expr
    # Pass
    # Break
    # Continue
    
# Creates a map from statement lines to the lines of the immediately-enclosing
# controller.
#
# TODO: Support unstructured control flow (break, continue, early return, etc.)
class ControlVisitor(ast.NodeVisitor):
    def __init__(self):
        # Maps statements to their immediate controllers
        self.line_to_controller = defaultdict(set)
        self.enclosing_controllers = [0] # 0 does not correspond to a statement.
        self.enclosing_loops = [0] # Index in enclosing_controllers of innermost loop
        self.break_guards = set() # Previous conditionals that may have avoided a break / continue
        self.break_lines = set() # Also continues
        self.handler_lines = set() # Exception handlers

    def die(self, node):
        raise ValueError('Unsupported node: ' + str(type(node)))

    def nothing(self, node):
        pass

    def visit(self, node):
        if isinstance(node, (ast.stmt, ast.excepthandler)):
            self.line_to_controller[node.lineno].add(self.enclosing_controllers[-1])
            self.line_to_controller[node.lineno] |= self.break_guards

        super(ControlVisitor, self).visit(node)

    def enclosed_visit(self, lineno, node):
        self.enclosing_controllers.append(lineno)
        self.visit(node)
        self.enclosing_controllers.pop()

    def enclosed_visits(self, lineno, nodes):
        self.enclosing_controllers.append(lineno)

        for node in nodes:
            self.visit(node)

        self.enclosing_controllers.pop()

    def loop_visit(self, lineno, node):
        # Don't clear the old ones, just remove the new ones afterwards
        old_break_guards = self.break_guards.copy()

        self.enclosing_loops.append(len(self.enclosing_controllers))
        self.enclosed_visit(lineno, node)
        self.enclosing_loops.pop()

        self.break_guards = old_break_guards

    def loop_visits(self, lineno, nodes):
        # Don't clear the old ones, just remove the new ones afterwards
        old_break_guards = self.break_guards.copy()

        self.enclosing_loops.append(len(self.enclosing_controllers))
        self.enclosed_visits(lineno, nodes)
        self.enclosing_loops.pop()

        self.break_guards = old_break_guards

    def visit_FunctionDef(self, stmt):
        self.enclosed_visits(stmt.lineno, stmt.body)

    visit_AsyncFunctionDef = die
    visit_ClassDef = visit_FunctionDef # TODO: handle class-specific things?

    # Return - TODO: Support early return

    def visit_IfLike(self, stmt):
        self.enclosed_visits(stmt.lineno, stmt.body)
        self.enclosed_visits(stmt.lineno, stmt.orelse)

    def visit_Loop(self, stmt):
        self.loop_visits(stmt.lineno, stmt.body)
        self.loop_visits(stmt.lineno, stmt.orelse)

    # TODO: {While, For} technically controls itself after the first iteration,
    # because it only executes if it didn't stop on the previous iteration
    visit_For = visit_Loop
    visit_AsyncFor = die
    visit_While = visit_Loop
    visit_If = visit_IfLike

    visit_With = die
    visit_AsyncWith = die

    visit_Raise = die
    def visit_Try(self, stmt):
        self.enclosed_visits(stmt.lineno, stmt.body)
        self.enclosed_visits(stmt.lineno, stmt.finalbody)
        self.enclosed_visits(stmt.lineno, stmt.handlers)

        # Does there need to be any special control handling for the else suite?
        self.enclosed_visits(stmt.lineno, stmt.orelse)

    def visit_ExceptHandler(self, handler):
        self.handler_lines.add(handler.lineno)
        self.enclosed_visits(handler.lineno, handler.body)

    def visit_Break(self, stmt):
        self.break_lines.add(stmt.lineno)
        self.break_guards |= set(self.enclosing_controllers[self.enclosing_loops[-1]:])

    visit_Continue = visit_Break

def used_stmt(exec_point, stmt):
    visitor = UseVisitor(exec_point)
    visitor.visit(stmt)
    return visitor.use_set

def defined_stmt(exec_point, next_exec_point):
    now_vars = VarEnvironment(exec_point)
    next_vars = VarEnvironment(next_exec_point)

    return now_vars.diff(next_vars)

# Returns a map from steps to lines and a combined UD and CT "multimap"
def build_relations(line_map, line_to_control, break_lines, handler_lines, tr):
    # UD instead of DU, so we can go use -> definition. Similarly, use CT
    # instead of TC
    UD_CT = defaultdict(set)
    UD_1 = {} # The most recent definitions, maps lines to lists of lines
    step_to_line = {}
    line_to_step = defaultdict(set)

    # Reference to step
    last_definitions = {}

    preceding_break = None # May also be a continue
    preceding_step = None
    preceding_excepting_step = None

    for step, exec_point in enumerate(tr):
        if exec_point['event'] not in ['step_line', 'exception', 'uncaught_exception']:
            continue

        line = exec_point['line']
        line_to_step[line].add(step)
        try:
            stmt = line_map[line]
        except KeyError:
            # TODO: this seems to happen only with decorators.
            # (other known causes like blank input and multi-line strings should
            # be handled in an earlier stage)
            raise BadInputException("decorators are not supported")

        step_to_line[step] = line

        # Use-Definition processing
        stmt_useds = used_stmt(exec_point,  stmt)
        new_ud = set()
        for ref in stmt_useds:
            if ref in last_definitions:
                new_ud.add(last_definitions[ref])
        UD_CT[step] |= new_ud
        UD_1[line] = [step_to_line[s] for s in new_ud]

        if exec_point['event'] == 'step_line':
            try:
                stmt_defineds = defined_stmt(tr[step], tr[step + 1])
            except IndexError:
                # TODO: find a better way of detecting this issue
                raise BadInputException("Insufficient raw input")
            for ref in stmt_defineds:
                last_definitions[ref] = step

        # Test-Control processing
        # TODO: Support control dependencies caused by function calls

        # Go through all the possible control dependencies and choose the most
        # recent one
        max_step = -1;
        controls = line_to_control[line] # Possible dependencies
        for control in controls:
            if line_to_step[control]:
                max_step = max([max_step, max(line_to_step[control])])
        if max_step >= 0:
            UD_CT[step].add(max_step)

        if preceding_break:
            UD_CT[step].add(preceding_break)

        if line in break_lines:
            preceding_break = step

        if line in handler_lines and preceding_excepting_step:
            UD_CT[step].add(preceding_excepting_step)

        if exec_point['event'] == 'step_line':
            preceding_step = step
        elif exec_point['event'] == 'exception':
            preceding_excepting_step = preceding_step

    return step_to_line, line_to_step, UD_CT, UD_1

def find_exception(trace):
    # Iterate reversed, because we want the *last* exception
    for step, exec_point in reversed(list(enumerate(trace))):
        if exec_point['event'] in ['uncaught_exception', 'exception']:
            if 'exception_msg' not in exec_point:
                print(exec_point)
                return (step, "")
            else:
                return (step, exec_point['exception_msg'])
    return (None, None)

span_rexp = re.compile('span_([0-9]*)_([0-9]*)_([0-9]*)_([0-9]*)')

"""
Returns a set of line numbers.

TODO: Guess or allow specification of specific values to track.
"""

def slice(source, ri, line=None, debug=False, tr=None, raw=False):
    line_map, line_to_control, break_lines, handler_lines = make_line_maps(source)
    if tr == None:
        tr = trace(source, ri)

    if not raw:
        bv = BindingVisitor()
        bv.visit(ast.parse(source))
        line_to_assignment = bv.line_to_assignment

    a = build_relations(line_map, line_to_control, break_lines, handler_lines, tr)
    step_to_line, line_to_step, UD_CT, UD_1 = a

    visited = set()
    queue = Queue()
    exception_step, exceptionMsg = find_exception(tr)

    if exception_step:
        if debug:
            print('Exception at line ' + str(step_to_line[exception_step]))
    elif not line:
        if raw:
            return None
        else:
            return None, None, None

    for step in line_to_step[line] if line else [exception_step]:
        queue.put(step)
    while not queue.empty():
        step = queue.get()
        if step in visited:
            continue
        visited.add(step)

        # Put influencing steps in the queue
        for infl_step in UD_CT[step]:
            queue.put(infl_step)

    keep_these = set([step_to_line[step] for step in visited])
    span_slice = []
    lines = source.split('\n')
    for lineNum in keep_these:
        if raw:
            line = lines[lineNum-1].split("#  interaction:")
            if len(line) == 2:
                span_slice.append(line[1])
            else:
                raise BadInputException("Sourcemap fail")
        else:
            if line in line_to_assignment:
                match = span_rexp.match(line_to_assignment[line])
                if match:
                    span_slice.append([int(s) for s in match.groups()])
    stmt_count = float(len(line_map))

    exceptionLine = step_to_line[exception_step]
    if raw:
        line = lines[exceptionLine-1].split("#  interaction:")
        if len(line) == 2:
            exceptionSpan = line[1]
        else:
            raise BadInputException("Sourcemap fail")

        return exceptionSpan, span_slice, exceptionMsg, UD_1
    else:
        return list(keep_these), len(keep_these) / stmt_count, span_slice

# Assumes one assignment per line max
class BindingVisitor(ast.NodeVisitor):
    def __init__(self):
        self.line_to_assignment = {}

    def visit_Assign(self, node):
        if (isinstance(node, ast.Assign) and len(node.targets) == 1):
            target = node.targets[0]
            if (isinstance(target, ast.Name)):
                line = target.lineno
                ident = target.id

                self.line_to_assignment[line] = ident


"""
Returns a dictionary mapping identifiers to types
"""

def extract_type_info(source, ri, tr=None):
    if tr == None:
        tr = trace(source, ri)

    bv = BindingVisitor()
    bv.visit(ast.parse(source))
    line_to_assignment = bv.line_to_assignment

    ident_to_type = {}

    for step, exec_point in enumerate(tr):
        if exec_point['event'] == 'step_line':
            line = exec_point['line']
            if line in line_to_assignment:
                ident = line_to_assignment[line]

                try:
                    next_vars = VarEnvironment(tr[step + 1])
                except IndexError:
                    # TODO: find a better way of detecting this issue
                    raise BadInputException("Insufficient raw input")

                val = next_vars.get_val(ident)
                if val:
                    if val[0] == 'HEAP_PRIMITIVE':
                        typ = val[1]
                    else:
                        typ = val[0]

                    if ident in ident_to_type and ident_to_type[ident] != typ:
                        ident_to_type[ident] = ident_to_type[ident] + ',' + typ
                    else:
                        ident_to_type[ident] = typ


    return ident_to_type

def type_and_slice(source, ri):
    tr = trace(source, ri)
    slice_info = slice(source, ri, tr=tr, raw=True)
    type_info = extract_type_info(source, ri, tr=tr)
    return (type_info, slice_info)
