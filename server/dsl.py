"""
ODE DSL
"""

import re
import json
import math
import collections
import dsl_parser

def indent_all_lines(count, text):
    prefix = " " * count
    return "\n".join(prefix + line for line in text.split("\n"))

def dict_insert_no_collision(d, desired_name, f):
    if desired_name not in d:
        obj = f(desired_name)
        d[desired_name] = obj
        return f(desired_name)
    suffix = 2
    while desired_name + str(suffix) in d:
        suffix += 1
    return dict_insert_no_collision(d, desired_name + str(suffix), f)

class InterpreterError(dsl_parser.SourcePositionError):
    NAME = "Interpreter"

LAYER_DYN = 1
LAYER_CONST = 2
LAYER_COMPTIME = 3

class Datum:
    IS_VAR = False
    LAYER = LAYER_DYN

class StateVariable(Datum):
    IS_VAR = True

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

    def get_deps(self):
        return {self}

class AdjustableParameter(Datum):
    LAYER = LAYER_CONST

    def __init__(self, name, default_value):
        self.name = name
        self.default_value = default_value

    def __repr__(self):
        return "param<%s>" % self.name

    def get_deps(self):
        return {self}

class Expr(Datum):
    def __init__(self, layer, op, args):
        assert layer in (1, 2, 3)
        self.LAYER = layer
        self.op = op
        self.args = args

    def __repr__(self):
        return "%s(%s)" % (self.op, ", ".join(map(repr, self.args)))

    def get_deps(self):
        r = set()
        for arg in self.args:
            r |= arg.get_deps()
        return r

class CompileTimeData(Datum):
    LAYER = LAYER_COMPTIME

    def __init__(self, ty, value):
        assert isinstance(value, ty)
        self.ty = ty
        self.value = value

    def __repr__(self):
        return repr(self.value)

    def get_deps(self):
        return set()

class Function:
    def __init__(self, name, args, return_type, body):
        self.name = name
        self.args = args
        self.return_type = return_type
        self.body = body

def func_print(ctx, scope):
    print("Print called:", scope["val"])

def func_Slider(ctx, scope):
    adj_var = ctx.make_adjustable_parameter(
        desired_name=scope["@purpose_name"],
        default_value=(scope["high"].value + scope["low"].value) / 2,
        name_must_be_exact=False,
    )
    ctx.widgets.append({
        "kind": "slider",
        "name": adj_var.name,
        "low": scope["low"].value,
        "high": scope["high"].value,
    })
    return adj_var

def func_Uniform(ctx, scope):
    return Expr(LAYER_CONST, "Uniform", [scope["low"], scope["high"]])

def func_Gaussian(ctx, scope):
    return Expr(LAYER_CONST, "Gaussian", [])

def func_WienerDerivative(ctx, scope):
    return Expr(LAYER_DYN, "WienerDerivative", [])

def prefix_join(a, b):
    if a == "":
        return b
    return a + "/" + b

class Context:
    def __init__(self):
        self.all_variables = {}
        self.adjustable_parameters = {}
        self.variable_initializers = {}
        self.variable_drivers = {}
        self.widgets = []
        self.root_scope = {
            "print": Function("print", [("val", None)], ("tuple", []), func_print),
            "Slider": Function("Slider", [("low", "float"), ("high", "float")], "const", func_Slider),
            "Uniform": Function("Uniform", [("low", "const"), ("high", "const")], "const", func_Uniform),
            "Gaussian": Function("Gaussian", [], "const", func_Gaussian),
            "WienerDerivative": Function("WienerDerivative", [], "dyn", func_WienerDerivative),
            "globalTime": Expr(LAYER_DYN, "globalTime", []),
            "globalStepSize": Expr(LAYER_DYN, "globalStepSize", []),
            "e": CompileTimeData(float, math.e),
            "pi": CompileTimeData(float, math.pi),
            "@name_prefix": "",
        }
        for name in ["exp", "log", "cos", "sin", "sqrt", "abs"]:
            # Due to Python closures just saving their enclosing scope by reference
            # we have to do this annoying trick with wrapping with another scope.
            def closure_scope(name):
                self.root_scope[name] = Function(
                    name=name,
                    args=[("x", "dyn")],
                    return_type="dyn",
                    body=lambda ctx, scope: Expr(LAYER_DYN, name, [scope["x"]]),
                )
            closure_scope(name)
        self.plots = {}
        self.settings = {
            "tolerance": 1e-2,
            "simtime": None,
        }
        self.most_recent_line_number = 1
        self.unique_counters = collections.defaultdict(int)

    def get_unique(self, name):
        self.unique_counters[name] += 1
        return "%s%i" % (name, self.unique_counters[name])

    def interpreter_error(self, message):
        return InterpreterError(
            message,
            dsl_parser.StreamPos(line_number=self.most_recent_line_number, column_number=1),
        )

    def global_interpreter_error(self, message):
        return InterpreterError(message, dsl_parser.StreamPos(-1, -1))

    def type_check_assert(self, value, ty):
        if ty is None:
            return
        if not isinstance(value, Datum):
            raise self.interpreter_error("Bug: Non-datum being type checked: %r : %r" % (value, ty))
        # Here float will accept int as a form of casting.
        concrete_mapping = {"int": {int}, "float": {float, int}, "str": {str}}
        print("===========", "Checking:", value, ty)
        # Everything can be a dyn.
        if ty == "dyn":
            return
        elif ty == "const":
            if not value.LAYER < LAYER_CONST:
                raise self.interpreter_error("Bad type, value should be const: %r : %r" % (value, ty))
        elif ty in concrete_mapping:
            if (not isinstance(value, CompileTimeData)) or value.ty not in concrete_mapping[ty]:
                raise self.interpreter_error("Bad type, value should be %s: %r" % (ty, value))
        else:
            raise self.interpreter_error("Bug: Unimplemented type in check: %r : %r" % (value, ty))

    def perform_compile_time_operator(self, op_name, args):
        if op_name == "+":
            result = sum(args)
        elif op_name == "-":
            if len(args) == 1:
                result = -args[0]
            else:
                result = args[0] - args[1]
        elif op_name == "*":
            result = args[0] * args[1]
        elif op_name == "/":
            result = args[0] / args[1]
        elif op_name == "%":
            result = args[0] % args[1]
        elif op_name == "^":
            result = args[0] ** args[1]
        else:
            raise self.interpreter_error("Bug: Unimplemented compile-time operation: %r(%s)" % (
                op_name, ", ".join(str(arg) for arg in args)
            ))
        return CompileTimeData(type(result), result)

    def evaluate_expr(self, scope, expr, purpose_name):
        kind = expr[0]
        if kind == "var":
            _, var_name = expr
            var_name = prefix_join(scope["@name_prefix"], var_name)
            if var_name in self.root_scope:
                value = self.root_scope[var_name]
                if not isinstance(value, Datum):
                    self.interpreter_error("Function %s can't be used like a variable" % var_name)
                return value
            self.register_variable(var_name)
            return self.all_variables[var_name]
        elif kind == "dot":
            # Right now we only support indexing global.
            _, lhs, name = expr
            if lhs != ("var", "global"):
                raise self.interpreter_error("For now global is the only thing that may appear on the left of a dot accessor")
            return self.evaluate_expr({"@name_prefix": ""}, ("var", name), "BUGBUGBUG")
        elif kind == "compvar":
            return self.lookup(scope, expr[1])
        elif kind == "lit":
            _, ty_name, value = expr
            return CompileTimeData({"int": int, "float": float, "str": str}[ty_name], value)
        elif kind in {"unary-op", "binary-op"}:
            op_name = expr[1]
            op_args = [self.evaluate_expr(scope, arg_expr, purpose_name) for arg_expr in expr[2:]]
            # Do constant folding here.
            if all(arg.LAYER == LAYER_COMPTIME for arg in op_args):
                return self.perform_compile_time_operator(op_name, [arg.value for arg in op_args])
            return Expr(
                layer=min(arg.LAYER for arg in op_args),
                op=op_name,
                args=op_args,
            )
        elif kind == "call":
            _, fn_expr, arg_exprs = expr
            if fn_expr[0] != "var":
                raise self.interpreter_error("For now only simple function calls are allowed")
            _, fn_name = fn_expr
            fn = self.lookup(scope, fn_name)
            if isinstance(fn, Datum):
                raise self.interpreter_error("%s isn't a function; do you want to drop the parens?" % fn_name)
            args = [self.evaluate_expr(scope, arg_expr, purpose_name) for arg_expr in arg_exprs]
            # Make sure that our arguments line up.
            if len(fn.args) != len(args):
                raise self.interpreter_error("Function %s expected %i arguments, we passed %i" % (
                    fn.name, len(fn.args), len(args),
                ))
            subscope = scope.copy()
            subscope["@purpose_name"] = purpose_name
            subscope["@name_prefix"] = prefix_join(scope["@name_prefix"], self.get_unique(fn_name))
            for arg, (arg_name, arg_ty_annot) in zip(args, fn.args):
                self.type_check_assert(arg, arg_ty_annot)
                subscope[arg_name] = arg
            if isinstance(fn.body, list):
                return self.execute(subscope, fn.body)
            else:
                return fn.body(self, subscope)
        raise self.interpreter_error("Bug! Unhandled expr: %r" % (expr,))

    def register_variable(self, name):
        if name not in self.all_variables:
            self.all_variables[name] = StateVariable(name)
        if name.endswith("'"):
            self.register_variable(name[:-1])

    def make_adjustable_parameter(self, desired_name, default_value, name_must_be_exact=True):
        if desired_name not in self.adjustable_parameters:
            adj_param = AdjustableParameter(desired_name, default_value)
            self.adjustable_parameters[desired_name] = adj_param
            return adj_param
        elif name_must_be_exact:
            raise self.interpreter_error("Attempt to redefine parameter: %s" % desired_name)
        return dict_insert_no_collision(self.adjustable_parameters, desired_name, AdjustableParameter)

    def compile_time_assign(self, scope, name, value):
        if name in scope:
            raise self.interpreter_error("Redefinition of %s" % (name,))
        scope[name] = value

    def lookup(self, scope, name):
        if name not in scope:
            raise self.interpreter_error("Undefined name: %s" % (name,))
        return scope[name]

    def set_initializer(self, lhs, rhs):
        if not lhs.IS_VAR:
            raise self.interpreter_error("LHS of ~ must be a var.")
        if not rhs.LAYER >= LAYER_CONST:
            raise self.interpreter_error("RHS of ~ must be constant.")
        # We add a prime here because if a variable is initialized then it's dynamic, and thus has a derivative.
        self.register_variable(lhs.name + "'")
        if lhs.name in self.variable_initializers:
            # TODO: Get both line numbers.
            raise self.interpreter_error("Attempt to double-initialize %s" % lhs.name)
        self.variable_initializers[lhs.name] = rhs

    def set_driver(self, lhs, rhs):
        if not lhs.IS_VAR:
            raise self.interpreter_error("LHS of <- must be a var.")
        self.register_variable(lhs.name)
        if lhs.name in self.variable_drivers:
            raise self.interpreter_error("Attempt to double-drive %s" % lhs.name)
        self.variable_drivers[lhs.name] = rhs

    def get_purpose_name_from(self, lhs):
        if lhs.IS_VAR:
            return lhs.name
        return "param"

    def execute(self, scope, program):
        if scope is None:
            scope = self.root_scope
        return_value = None
        # Actually execute the contained statements.
        for statement in program:
            kind = statement[0]
            if kind == "line":
                _, line_number = statement
                self.most_recent_line_number = line_number
            elif kind == "fn":
                _, fn_desc = statement
                self.compile_time_assign(
                    scope,
                    fn_desc["name"],
                    Function(
                        name=fn_desc["name"],
                        args=fn_desc["args"],
                        return_type=fn_desc["return_type"],
                        body=fn_desc["body"],
                    ),
                )
            elif kind == "plot":
                _, var_exprs = statement
                for var_expr in var_exprs:
                    val = self.evaluate_expr(scope, var_expr, "plot")
                    desired_name = "plot"
                    # If we depend on just one variable then auto-name based on that.
                    if len(val.get_deps()) == 1:
                        desired_name = val.get_deps().pop().name
                    dict_insert_no_collision(self.plots, desired_name, lambda final_name: {
                        "name": final_name,
                        "expr": val,
                    })
            elif kind in {"tolerance", "simtime"}:
                _, value = statement
                self.settings[kind] = value
            elif kind == "expr":
                _, e = statement
                expr_kind = e[0]
                if expr_kind == "binary-op" and e[1] in {"~", "<-"}:
                    _, op_kind, lhs, rhs = e
                    lhs = self.evaluate_expr(scope, lhs, "param")
                    purpose_name = self.get_purpose_name_from(lhs) + {"~": "_init", "<-": "_param"}[op_kind]
                    rhs = self.evaluate_expr(scope, rhs, purpose_name=purpose_name)
                    if op_kind == "~":
                        self.set_initializer(lhs, rhs)
                    elif op_kind == "<-":
                        self.set_driver(lhs, rhs)
                elif expr_kind == "call":
                    result = self.evaluate_expr(scope, e, "param")
                else:
                    raise self.interpreter_error("Invalid expression. Only (x ~ y), (x <- y), and function calls allowed at top-level.")
            elif kind == "javascript":
                raise self.interpreter_error("Inline javascript not currently fully implemented.")
            elif kind == "return":
                _, e = statement
                if e is None:
                    return
                return self.evaluate_expr(scope, e, "param")
            else:
                raise self.interpreter_error("Bug! Unhandled statement: %r" % (statement,))

    def codegen_shared(self):
        self.codegen_variable_info = {}
        # Start by sanity-checking everything.
        variable_orders = collections.defaultdict(int)
        for var in self.all_variables:
            base_name, ticks = re.match("([^']*)('*)", var).groups()
            order = len(ticks)
            variable_orders[base_name] = max(variable_orders[base_name], order)
            # Fill up codegen_variable_info with information about each base variable.
            if base_name not in self.codegen_variable_info:
                self.codegen_variable_info[base_name] = {}
            self.codegen_variable_info[base_name]["order"] = variable_orders[base_name]
        # Here are the rules:
        # If a variable is of order k then:
        # 1. No derivatives of order 0 through k-1 may be driven.
        # 2. The derivative of order k must be driven.
        # 3. Initializers are completely unconstrained, because they default to 0.
        for base_name, order in variable_orders.items():
            top_derivative = base_name + "'" * order
            if top_derivative not in self.variable_drivers:
                raise self.global_interpreter_error("Variable %s isn't driven." % top_derivative)
            self.codegen_variable_info[base_name]["driver"] = self.variable_drivers[top_derivative]
            self.codegen_variable_info[base_name]["initializers"] = []
            for i in range(order):
                lower_derivative = base_name + "'" * i
                if lower_derivative in self.variable_drivers:
                    raise self.global_interpreter_error("Variable %s is driven, when %s should be." % (
                        lower_derivative, top_derivative,
                    ))
                self.codegen_variable_info[base_name]["initializers"].append(
                    self.variable_initializers.get(lower_derivative, CompileTimeData(int, 0)),
                )
        import pprint
        pprint.pprint(self.codegen_variable_info)
        # Zeroth order variables are a little strange.
        # To implement their instantness we topologically sort them here.
        zeroth_order_variables = {base_name for base_name, order in variable_orders.items() if order == 0}
        zeroth_order_dependencies = {}
        for zo_var in zeroth_order_variables:
            zeroth_order_dependencies[zo_var] = set()
            for dep in self.variable_drivers[zo_var].get_deps():
                if isinstance(dep, StateVariable):
                    zeroth_order_dependencies[zo_var].add(dep.name)
        # Do the topological sort.
        self.zo_topo_sorted = []
        warning_marks = set()
        unvisited = zeroth_order_variables.copy()
        def visit(node):
            if node not in unvisited:
                return
            if node in warning_marks:
                raise self.global_interpreter_error("Cyclic dependencies in zeroth order variables involving: %s" % node)
            warning_marks.add(node)
            for child in zeroth_order_dependencies[node]:
                visit(child)
            warning_marks.remove(node)
            unvisited.remove(node)
            self.zo_topo_sorted.append(node)
        while unvisited:
            visit(next(iter(unvisited)))
        assert len(self.zo_topo_sorted) == len(zeroth_order_variables)
        # Compute our buffer layouts.
        self.scratch_buffer_size = sum(info["order"] == 0 for info in self.codegen_variable_info.values())
        self.degrees_of_freedom = sum(info["order"] for info in self.codegen_variable_info.values())
        self.scratch_allocation = {zo: i for i, zo in enumerate(self.zo_topo_sorted)}
        self.slot_allocation = {}
        for base_name in sorted(self.codegen_variable_info.keys()):
            info = self.codegen_variable_info[base_name]
            for i in range(info["order"]):
                self.slot_allocation[base_name + "'" * i] = len(self.slot_allocation)
        self.parameter_allocation = {param: i for i, param in enumerate(sorted(self.adjustable_parameters))}
        #print("codegen_variable_info:", self.codegen_variable_info)
        #print("zo_topo_sorted:", self.zo_topo_sorted)
        #print("scratch_buffer_size:", self.scratch_buffer_size)
        #print("degrees_of_freedom:", self.degrees_of_freedom)
        #print("scratch_allocation:", self.scratch_allocation)
        #print("slot_allocation:", self.slot_allocation)
        # Compute the initial state of our plot data.
        self.plot_data_initial = {}
        self.plots_desc = {}
        for plot_name, plot_spec in self.plots.items():
            self.plot_data_initial[plot_name] = {"x": [], "y": []}
            self.plots_desc[plot_name] = {}

    def codegen_js_expr(self, expr):
        if isinstance(expr, StateVariable):
            if expr.name in self.scratch_allocation:
                return "scratch[%i /*%s*/]" % (self.scratch_allocation[expr.name], expr.name)
            return "state[%i /*%s*/]" % (self.slot_allocation[expr.name], expr.name)
        elif isinstance(expr, AdjustableParameter):
            return "parameters[%i /*%s*/]" % (self.parameter_allocation[expr.name], expr.name)
        elif isinstance(expr, Expr):
            args = [self.codegen_js_expr(arg) for arg in expr.args]
            if expr.op == "globalTime":
                return "t"
            if expr.op == "globalStepSize":
                return "dt"
            if expr.op == "WienerDerivative":
                return "wienerDerivative(dt)"
            fn_table = {
                "Uniform": "uniformRandom",
                "Gaussian": "gaussianRandom",
                "^": "Math.pow",
                "exp": "Math.exp",
                "log": "Math.log",
                "sin": "Math.sin",
                "cos": "Math.cos",
                "sqrt": "Math.sqrt",
                "abs": "Math.abs",
            }
            pass_through_operators = {"+", "-", "*", "/", "%"}
            if expr.op in fn_table:
                return fn_table[expr.op] + "(%s)" % ", ".join(args)
            elif expr.op in pass_through_operators:
                if len(args) == 1:
                    return "(%s %s)" % (expr.op, args[0])
                if len(args) == 2:
                    return "(%s %s %s)" % (args[0], expr.op, args[1])
        elif isinstance(expr, CompileTimeData):
            return "%r" % (expr.value,)
        raise self.interpreter_error("Bug: Unhandled expr in JS codegen: %r" % (expr,))

    def codegen_js(self):
        self.codegen_shared()

        allocate_code = []
        for param_name, param in self.adjustable_parameters.items():
            allocate_code.append("parameters[%s /*%s*/] = %r;" % (
                self.parameter_allocation[param_name], param_name, param.default_value,
            ))
        allocate_code = "\n".join(allocate_code)

        init_code = []
        for base_name in sorted(self.codegen_variable_info.keys()):
            info = self.codegen_variable_info[base_name]
            init_code.append("// " + base_name)
            for i, initializer in enumerate(info["initializers"]):
                v = base_name + "'" * i
                init_code.append("state[%s /*%s*/] = %s;" % (
                    self.slot_allocation[v], v, self.codegen_js_expr(initializer),
                ))
        init_code = "\n".join(init_code)

        derivative_code = ["// === Compute zeroth order ==="]
        for zo in self.zo_topo_sorted:
            derivative_code.append("scratch[%s /*%s*/] = %s;" % (
                self.scratch_allocation[zo], zo, self.codegen_js_expr(self.codegen_variable_info[zo]["driver"]),
            ))
        derivative_code.append("// === Compute derivatives ===")
        for base_name in sorted(self.codegen_variable_info.keys()):
            info = self.codegen_variable_info[base_name]
            if info["order"] == 0:
                continue
            derivative_code.append("// " + base_name)
            for i in range(info["order"] - 1):
                v = base_name + "'" * i
                derivative_code.append("statePrime[%s /*%s*/] = state[%s /*%s*/]; // Implicit" % (
                    self.slot_allocation[v], v, self.slot_allocation[v + "'"], v + "'",
                ))
            top_state = base_name + "'" * (info["order"] - 1)
            derivative_code.append("statePrime[%s /*%s*/] = %s;" % (
                self.slot_allocation[top_state], top_state, self.codegen_js_expr(info["driver"]),
            ))
        derivative_code = "\n".join(derivative_code)

        extract_plot_datum = []
        for plot_name, plot_spec in self.plots.items():
            extract_plot_datum.append("plotData[%s].x.push(t);" % repr(plot_name))
            extract_plot_datum.append("plotData[%s].y.push(%s);" % (
                repr(plot_name), self.codegen_js_expr(plot_spec["expr"]),
            ))
        extract_plot_datum = "\n".join(extract_plot_datum)

        subst = {
            "degrees_of_freedom": self.degrees_of_freedom,
            "scratch_buffer_size": self.scratch_buffer_size,
            "parameter_count": len(self.adjustable_parameters),
            "allocate_code": indent_all_lines(12, allocate_code).strip(),
            "initialization_code": indent_all_lines(12, init_code).strip(),
            "derivative_code": indent_all_lines(12, derivative_code).strip(),
            "extract_plot_datum": indent_all_lines(12, extract_plot_datum).strip(),
            "plot_data_initial": indent_all_lines(12, json.dumps(self.plot_data_initial, indent=4)).strip(),
            "plots": indent_all_lines(8, json.dumps(self.plots_desc, indent=4)).strip(),
            "widgets": indent_all_lines(8, json.dumps(self.widgets, indent=4)).strip(),
            "parameter_table": json.dumps(self.parameter_allocation),
            "scratch_table": json.dumps(self.scratch_allocation),
            "state_table": json.dumps(self.slot_allocation),
            "settings": json.dumps(self.settings),
        }
        return """
(() => {
    let xoshiro128ss_state = [[1, 2, 3, 4]];
    const reseedRNG = () => {
        const newState = [];
        for (let i = 0; i < 4; i++)
            newState.push(Math.floor(Math.random() * 4294967296) | 0);
        xoshiro128ss_state[0] = newState;
    }
    reseedRNG();
    function myRandom() {
        let [a, b, c, d] = xoshiro128ss_state[0];
        var t = b << 9, r = a * 5; r = (r << 7 | r >>> 25) * 9;
        c ^= a; d ^= b;
        b ^= c; a ^= d; c ^= t;
        d = d << 11 | d >>> 21;
        xoshiro128ss_state[0] = [a, b, c, d];
        return (r >>> 0) / 4294967296;
    }
    function uniformRandom(low, high) {
        return low + (high - low) * myRandom();
    }
    const boxMullerCache = [null];
    function gaussianRandom() {
        if (boxMullerCache[0] === null) {
            // Use Box-Muller transform, and stash the other sample for later.
            const u = 1e-50 + myRandom(), v = myRandom();
            const scale = Math.sqrt(-2 * Math.log(u));
            const arg = 2 * Math.PI * v;
            boxMullerCache[0] = scale * Math.cos(arg);
            return scale * Math.sin(arg);
        }
        const val = boxMullerCache[0];
        boxMullerCache[0] = null;
        return val;
    }
    function wienerDerivative(dt) {
        return gaussianRandom() / Math.sqrt(dt);
    }
    return {
        allocate: () => {
            const state = new Float64Array(%(degrees_of_freedom)s);
            const statePrime = new Float64Array(%(degrees_of_freedom)s);
            const scratch = new Float64Array(%(scratch_buffer_size)s);
            const parameters = new Float64Array(%(parameter_count)s);
            const plotData = {};
            %(allocate_code)s
            return {state, statePrime, scratch, parameters, plotData};
        },
        initialize: (ctx) => {
            ctx.plotData = %(plot_data_initial)s;
            const {state, scratch, parameters} = ctx;
            %(initialization_code)s
        },
        getDerivative: (ctx, t, dt) => {
            const {state, statePrime, scratch, parameters} = ctx;
            %(derivative_code)s
        },
        extractPlotDatum: (ctx, t, dt) => {
            const {state, statePrime, scratch, parameters, plotData} = ctx;
            %(extract_plot_datum)s
        },
        reseedRNG,
        getRNGState: () => [...xoshiro128ss_state[0]],
        setRNGState: (state) => {
            xoshiro128ss_state[0] = [...state];
        },
        plots: %(plots)s,
        widgets: %(widgets)s,
        parameterTable: %(parameter_table)s,
        // These two tables aren't used by the demo app, but you might want to use yourself.
        zerothOrderTable: %(scratch_table)s,
        stateTable: %(state_table)s,
        settings: %(settings)s,
    };
})()
""" % subst

if __name__ == "__main__":
    src = """
fn index($x: [dyn], $i: int) -> dyn {
    if $i < 0 || $i >= len($x) {
        return 0.0;
    }
    return $x[$i];
}

fn second_order_upwind($x: [dyn], $i: int) -> dyn {
    return
      + 3*index($x, $i - 1)
      - 4*index($x, $i + 0)
      +   index($x, $i + 1);
}

foo <- Slider(1, 2);
freq <- foo;
x ~ Uniform(-1, 1);
x'' <- -freq * x;

plot x;
simtime 10;
"""
    try:
        ast = dsl_parser.parse(src)
    except dsl_parser.SourcePositionError as e:
        print("===== Position:", e.stream_pos.line_number, e.stream_pos.column_number)
        raise
    ctx = Context()
    ctx.execute(None, ast)
    print("Variables:", ctx.all_variables)
    print("Initializers:")
    for var in ctx.all_variables:
        print(var, "~", ctx.variable_initializers.get(var, "(0)"))
    print("Drivers:")
    for var in ctx.all_variables:
        print(var, "<-", ctx.variable_drivers.get(var, "(0)"))
    js = ctx.codegen_js()
    print("========== Generated code:")
    print(js)
