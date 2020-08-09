"""
ODE DSL
"""

import re
import json
import math
import copy
import collections
import dsl_parser
import dsl_units

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

class ArrayVariable(Datum):
    IS_VAR = False # We need to be indexed to make a var.

    def __init__(self, name, length):
        self.name = name
        self.length = length

    def __repr__(self):
        return "<array %s of length %i>" % (self.name, self.length)

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
    def __init__(self, name, args, return_type, body, do_prefix_names=True, named_args=None):
        self.name = name
        self.args = args
        self.return_type = return_type
        self.body = body
        self.do_prefix_names = do_prefix_names
        self.named_args = named_args or {}

def func_print(ctx, scope):
    m = " ".join(str(i) for i in scope["@args"])
    print("Print called:", m)
    ctx.print_output.append(m)

def func_Slider(ctx, scope):
    args, opt_args = scope["@args"], scope["@optional_args"]
    if len(args) == 2:
        ctx.type_check_assert(args[0], "float")
        ctx.type_check_assert(args[1], "float")
        desired_name = scope["@purpose_name"]
        low, high = [arg.value for arg in args]
    elif len(args) == 3:
        ctx.type_check_assert(args[0], "str")
        ctx.type_check_assert(args[1], "float")
        ctx.type_check_assert(args[2], "float")
        desired_name, low, high = [arg.value for arg in args]
    default_value = (low + high) / 2
    print("OPTIONALS:", opt_args)
    if "$default" in opt_args:
        default_value = opt_args["$default"].value
    print("DEFAULT VALUE:", default_value)
    adj_var = ctx.make_adjustable_parameter(
        desired_name=prefix_join(scope["@name_prefix"], desired_name),
        default_value=default_value,
        name_must_be_exact=False,
    )
    is_log = opt_args["$log"].value if "$log" in opt_args else False
    if is_log and (low <= 0 or high <= 0):
        ctx.interpreter_error("Non-positive limit on log Slider")
    ctx.widgets.append({
        "kind": "slider",
        "name": adj_var.name,
        "low": low,
        "high": high,
        "log": is_log,
        "format": opt_args["$format"].value if "$format" in opt_args else None,
        "default_value": default_value,
        "recomp": False,
    })
    return adj_var

def func_Checkbox(ctx, scope):
    args, opt_args = scope["@args"], scope["@optional_args"]
    if len(args) == 0:
        desired_name = scope["@purpose_name"]
    else:
        ctx.type_check_assert(args[0], "str")
        desired_name = args[0].value
    default_value = False
    if "$default" in opt_args:
        default_value = bool(opt_args["$default"].value)
    comp_param, result = ctx.get_compilation_parameter(
        desired_name=prefix_join(scope["@name_prefix"], desired_name),
        default_value=default_value,
    )
    ctx.widgets.append({
        "kind": "checkbox",
        "name": comp_param,
        "default_value": default_value,
        "recompile": True,
    })
    return CompileTimeData(bool, result)

def func_Selector(ctx, scope):
    args, opt_args = scope["@args"], scope["@optional_args"]
    # Check if the first argument is a string.
    if args[0].LAYER != LAYER_COMPTIME:
        ctx.interpreter_error("All arguments to Selector must be compile time")
    if args[0].ty == str:
        desired_name = args.pop(0).value
    else:
        desired_name = scope["@purpose_name"]
    # Take the remaining arguments and select over them.
    for arg in args:
        ctx.type_check_assert(arg, "Function")
    default_value = 0
    if "$default" in opt_args:
        default_value = bool(opt_args["$default"].value)
    comp_param, result = ctx.get_compilation_parameter(
        desired_name=prefix_join(scope["@name_prefix"], desired_name),
        default_value=default_value,
    )
    ctx.widgets.append({
        "kind": "selector",
        "name": comp_param,
        "selections": [arg.value.name for arg in args],
        "default_value": default_value,
        "recompile": True,
    })
    return args[result] #CompileTimeData(type(args[result]), args[result])

def func_Uniform(ctx, scope):
    return ctx.constify_via_param("Uniform", Expr(LAYER_CONST, "Uniform", [scope["low"], scope["high"]]))

def func_Gaussian(ctx, scope):
    return ctx.constify_via_param("Gaussian", Expr(LAYER_CONST, "Gaussian", []))

def func_Gamma(ctx, scope):
    if scope["alpha"].value <= 0 or scope["beta"].value <= 0:
        ctx.interpreter_error("Gamma(ɑ, β) requires ɑ, β > 0")
    return ctx.constify_via_param("Gamma", Expr(LAYER_CONST, "Gamma", [scope["alpha"], scope["beta"]]))

def func_Beta(ctx, scope):
    if scope["alpha"].value <= 0 or scope["beta"].value <= 0:
        ctx.interpreter_error("Beta(ɑ, β) requires ɑ, β > 0")
    return ctx.constify_via_param("Beta", Expr(LAYER_CONST, "Beta", [scope["alpha"], scope["beta"]]))

def func_Frechet(ctx, scope):
    if scope["alpha"].value <= 0:
        ctx.interpreter_error("Frechet(ɑ) requires ɑ > 0")
    return ctx.constify_via_param("Frechet", Expr(LAYER_CONST, "Frechet", [scope["alpha"]]))

def func_addDeriv(ctx, scope):
    var = scope["var"]
    ctx.register_variable(var.name + "'")
    return ctx.all_variables[var.name + "'"]

def func_subDeriv(ctx, scope):
    var = scope["var"]
    base_name, ticks = split_variable_name(var.name)
    if not ticks:
        ctx.interpreter_error("subDeriv called on already zeroth order variable: %s" % base_name)
    return ctx.all_variables[base_name + ticks[1:]]

def func_PoissonProcess(ctx, scope):
    result = Expr(LAYER_DYN, "RealizedProcess", [])
    result.realized_index = ctx.make_realized_process("PoissonProcess", [scope["rate"]])
    return result

def func_WienerProcess(ctx, scope):
    result = Expr(LAYER_DYN, "RealizedProcess", [])
    result.realized_index = ctx.make_realized_process("WienerProcess", [])
    return result

def func_WienerDerivative(ctx, scope):
    result = Expr(LAYER_DYN, "RealizedProcess", [])
    result.realized_index = ctx.make_realized_process("WienerDerivative", [])
    return result

def func_WienerDerivativeUnstable(ctx, scope):
    return Expr(LAYER_DYN, "WienerDerivativeUnstable", [])

def func_len(ctx, scope):
    return CompileTimeData(int, scope["arr"].length)

def func_str(ctx, scope):
    obj = scope["obj"]
    s = obj.name if hasattr(obj, "name") else str(obj)
    return CompileTimeData(str, s)

def prefix_join(a, b):
    if a == "":
        return b
    return a + "/" + b

def split_variable_name(var_name):
    return re.match("([^']*)('*)", var_name).groups()

SIMPLE_PLOT_DEFAULT_LAYOUT = {
    # title is filled in where this is used.
    "width": 500,
    "height": 300,
    "margin": {"l": 30, "r": 30, "b": 30, "t": 30, "pad": 4},
    "plot_bgcolor": "#eee",
    "paper_bgcolor": "#eee",
}
DEFAULT_PLOT_DATA_SETTINGS = {
    "type": "scatter",
    "mode": "lines",
}
DEFAULT_HEATMAP_DATA_SETTINGS = {
    "type": "heatmap",
}

class Context:
    def __init__(self, external_compilation_parameters=None):
        self.external_compilation_parameters = external_compilation_parameters.copy() or {}
        self.all_variables = {}
        self.array_variables = {}
        self.adjustable_parameters = {}
        self.expr_initialized_parameters = {}
        self.compilation_parameters = {}
        self.variable_initializers = {}
        self.variable_drivers = {} # name: str -> value
        self.realized_processes = [] # process_name: str
        self.widgets = []
        self.print_output = []
        self.root_scope = {
            "print": Function("print", 0, ("tuple", []), func_print),
            "Slider": Function(
                "Slider", 2, "const", func_Slider, do_prefix_names=False,
                named_args={"$default": "float", "$log": "bool", "$format": "str"},
            ),
            "Checkbox": Function("Checkbox", 0, "int", func_Checkbox, do_prefix_names=False, named_args={"$default": "int"}),
            "Selector": Function("Selector", 1, None, func_Selector, do_prefix_names=False, named_args={"$default": "int"}),
            "Uniform": Function("Uniform", [("low", "float"), ("high", "float")], "const", func_Uniform),
            "Gaussian": Function("Gaussian", [], "const", func_Gaussian),
            "Gamma": Function("Gamma", [("alpha", "float"), ("beta", "float")], "const", func_Gamma),
            "Beta": Function("Beta", [("alpha", "float"), ("beta", "float")], "const", func_Beta),
            "Frechet": Function("Frechet", [("alpha", "float")], "const", func_Frechet),
            "addDeriv": Function("addDeriv", [("var", "var")], "var", func_addDeriv),
            "subDeriv": Function("subDeriv", [("var", "var")], "var", func_subDeriv),
            "PoissonProcess": Function("PoissonProcess", [("rate", "float")], "dyn", func_PoissonProcess),
            "WienerProcess": Function("WienerProcess", [], "dyn", func_WienerProcess),
            "WienerDerivative": Function("WienerDerivative", [], "dyn", func_WienerDerivative),
            "WienerDerivativeUnstable": Function("WienerDerivativeUnstable", [], "dyn", func_WienerDerivativeUnstable),
            "len": Function("len", [("arr", ("list", "dyn"))], "int", func_len),
            "str": Function("str", [("obj", None)], "int", func_str),
            "globalTime": Expr(LAYER_DYN, "globalTime", []),
            "globalStepSize": Expr(LAYER_DYN, "globalStepSize", []),
            "e": CompileTimeData(float, math.e),
            "pi": CompileTimeData(float, math.pi),
            "false": CompileTimeData(bool, False),
            "true": CompileTimeData(bool, True),
            # Values used internally by the interpreter.
            "@name_prefix": "",
            "@current_plot": None,
        }
        self.always_global_variables = {"globalTime", "globalStepSize", "e", "pi", "true", "false"}
        for name, comp_time_func in {
            "exp": math.exp,
            "log": math.log,
            "cos": math.cos,
            "sin": math.sin,
            "sqrt": math.sqrt,
            "abs": abs,
            "floor": math.floor,
            "ceil": math.ceil,
            "round": round,
        }.items():
            # Due to Python closures just saving their enclosing scope by reference
            # we have to do this annoying trick with wrapping with another scope.
            def closure_scope(name, comp_time_func):
                def body_func(ctx, scope):
                    arg = scope["x"]
                    if arg.LAYER == LAYER_COMPTIME:
                        result = comp_time_func(arg.value)
                        return CompileTimeData(type(result), result)
                    return Expr(arg.LAYER, name, [arg])
                self.root_scope[name] = Function(
                    name=name,
                    args=[("x", "dyn")],
                    return_type="dyn",
                    body=body_func,
                )
            closure_scope(name, comp_time_func)
        for name in ["min", "max"]:
            def closure_scope(name):
                self.root_scope[name] = Function(
                    name=name,
                    args=1,
                    return_type="dyn",
                    body=lambda ctx, scope: Expr(LAYER_DYN, name, scope["@args"]),
                )
            closure_scope(name)
        self.plots = {}
        self.settings = {
            "integrator": "cash-karp",
            "tolerance": 1e-6,
            "stepsize": 0.1,
            "minstep": None,
            "maxstep": None,
            "plotperiod": 1e-2,
            "simtime": 10.0,
            "mcsamples": 1,
            "mctraces": 100,
            "mcenvelope": False,
            "processscale": 1e-2,
            "randomseed": None,
        }
        self.unit_system = dsl_units.UnitSystem(self.interpreter_error)
        self.most_recent_line_number = 1
        self.unique_counters = collections.defaultdict(int)

    def get_unique(self, name):
        self.unique_counters[name] += 1
        return "%s%i" % (name, self.unique_counters[name])

    def interpreter_error(self, message):
        raise InterpreterError(
            message,
            dsl_parser.StreamPos(line_number=self.most_recent_line_number, column_number=1),
        )

    def global_interpreter_error(self, message):
        raise InterpreterError(message, dsl_parser.StreamPos(-1, -1))

    def type_check_assert(self, value, ty):
        if ty is None:
            return
        if not isinstance(value, Datum):
            self.interpreter_error("Bug: Non-datum being type checked: %r : %r" % (value, ty))
        # Here float will accept int as a form of casting.
        concrete_mapping = {
            "int": {bool, int},
            "float": {float, int},
            "bool": {bool, int},
            "str": {str},
            "Function": {Function},
        }

        # Handle the array case.
        if isinstance(ty, tuple) and ty[0] == "list":
            if ty not in {("list", "dyn"), ("list", "var")}:
                self.interpreter_error("Unhandled array type [%s], for now only [dyn] and [var] are supported" % (ty[1],))
            if not isinstance(value, ArrayVariable):
                self.interpreter_error("Type check failure, value should be an array: %r : %r" % (value, ty))
            return

        # Handle all scalar cases
        if isinstance(value, ArrayVariable):
            self.interpreter_error("Type check failure, value shouldn't be an array: %r : %r" % (value, ty))
        elif ty == "dyn":
            # Everything can be a dyn.
            return
        elif ty == "var":
            if not value.IS_VAR:
                self.interpreter_error("Type check failure, value should be a variable: %r : %r" % (value, ty))
        elif ty == "const":
            if value.LAYER < LAYER_CONST:
                self.interpreter_error("Type check failure, value should be const: %r : %r" % (value, ty))
        elif ty in concrete_mapping:
            if (not isinstance(value, CompileTimeData)) or value.ty not in concrete_mapping[ty]:
                self.interpreter_error("Type check failure, value should be %s: %r" % (ty, value))
        else:
            self.interpreter_error("Bug: Unimplemented type in check: %r : %r" % (value, ty))

    def overlay_json(self, dest, source):
        if not isinstance(dest, dict):
            self.interpreter_error("Bug: Overlay target isn't a dict? %r" % (dest,))
        if not isinstance(source, dict):
            self.interpreter_error("Overlay must be a dict, not: %r" % (source,))
        for k, v in source.items():
            if k in dest and isinstance(dest[k], dict) and isinstance(v, dict):
                self.overlay_json(dest[k], v)
            else:
                dest[k] = v

    def perform_compile_time_operator(self, op_name, args):
        if op_name == "+":
            if len(args) == 1:
                result = args[0]
            else:
                result = args[0] + args[1]
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
        elif op_name == "..":
            result = args[0], args[1]
        elif op_name == "||":
            result = args[0] or args[1]
        elif op_name == "&&":
            result = args[0] or args[1]
        elif op_name == "<":
            result = args[0] < args[1]
        elif op_name == ">":
            result = args[0] > args[1]
        elif op_name == "<=":
            result = args[0] <= args[1]
        elif op_name == ">=":
            result = args[0] >= args[1]
        elif op_name == "==":
            result = args[0] == args[1]
        elif op_name == "!=":
            result = args[0] != args[1]
        else:
            self.interpreter_error("Bug: Unimplemented compile-time operation: %r(%s)" % (
                op_name, ", ".join(str(arg) for arg in args)
            ))
        return CompileTimeData(type(result), result)

    def evaluate_expr(self, scope, expr, purpose_name):
        kind = expr[0]
        if kind == "var":
            _, var_name = expr
            # Check if the variable is exempt from namespacing due to always being global.
            if var_name not in self.always_global_variables:
                var_name = prefix_join(scope["@name_prefix"], var_name)
            # Check if this is an array variable.
            base_var_name, _ = split_variable_name(var_name)
            if base_var_name in self.array_variables:
                return ArrayVariable(var_name, self.array_variables[base_var_name].length)
            elif var_name in self.root_scope:
                value = self.root_scope[var_name]
                if not isinstance(value, Datum):
                    assert isinstance(value, Function)
                    value = CompileTimeData(type(value), value)
                    #self.interpreter_error("Function %s can't be used like a variable" % var_name)
                return value
            self.register_variable(var_name)
            return self.all_variables[var_name]
        elif kind == "prime":
            _, var_expr = expr
            var = self.evaluate_expr(scope, var_expr, purpose_name)
            self.type_check_assert(var, "var")
            new_name = var.name + "'"
            self.register_variable(new_name)
            return self.all_variables[new_name]
        elif kind == "dot":
            # Right now we only support indexing global.
            _, lhs, name = expr
            if lhs != ("var", "global"):
                self.interpreter_error("For now global is the only thing that may appear on the left of a dot accessor")
            return self.evaluate_expr({"@name_prefix": ""}, ("var", name), "BUGBUGBUG")
        elif kind == "compvar":
            return self.lookup(scope, expr[1])
        elif kind == "lit":
            _, ty_name, value = expr
            return CompileTimeData({"int": int, "float": float, "str": str}[ty_name], value)
        elif kind in {"unary-op", "binary-op"}:
            op_name = expr[1]
            op_args = [self.evaluate_expr(scope, arg_expr, purpose_name) for arg_expr in expr[2:]]
            if op_name == "[]":
                array_var, index = op_args
                if not isinstance(array_var, ArrayVariable):
                    self.interpreter_error("Indexing must be done on an array variable, not: %r" % (array_var,))
                if not index.LAYER == LAYER_COMPTIME and index.ty == int:
                    self.interpreter_error("Index into %r must be a compile-time integer, not: %r" % (array_var, index))
                if not (0 <= index.value < array_var.length):
                    self.interpreter_error("Index %i out of range for %r" % (index.value, array_var))
                array_var_base, array_var_ticks = split_variable_name(array_var.name)
                magic_name = "%s[%i]%s" % (array_var_base, index.value, array_var_ticks)
                self.register_variable(magic_name)
                return self.all_variables[magic_name]
            # Do constant folding here.
            if all(arg.LAYER == LAYER_COMPTIME for arg in op_args):
                return self.perform_compile_time_operator(op_name, [arg.value for arg in op_args])
            return Expr(
                layer=min(arg.LAYER for arg in op_args),
                op=op_name,
                args=op_args,
            )
        elif kind == "call":
            _, fn_expr, arg_exprs, named_arg_exprs = expr
            if fn_expr[0] == "var":
                _, fn_name = fn_expr
                fn = self.lookup(scope, fn_name)
            else:
                fn_obj = self.evaluate_expr(scope, fn_expr, purpose_name)
                self.type_check_assert(fn_obj, "Function")
                fn = fn_obj.value
                fn_name = fn.name
                #self.interpreter_error("For now only simple function calls are allowed")
            if isinstance(fn, Datum):
                self.interpreter_error("%s isn't a function; do you want to drop the parens?" % fn_name)
            args = [self.evaluate_expr(scope, arg_expr, purpose_name) for arg_expr in arg_exprs]
            named_args = {
                name: self.evaluate_expr(scope, arg_expr, purpose_name)
                for name, arg_expr in named_arg_exprs.items()
            }
            # If fn.args is a number, then it's a minimum number of arguments, and there is no type-safety.
            if isinstance(fn.args, int):
                if len(args) < fn.args:
                    self.interpreter_error("Function %s expected at least %i arguments, we passed %i" % (
                        fn.name, fn.args, len(args),
                    ))
            elif len(fn.args) != len(args):
                # Make sure that our arguments line up.
                self.interpreter_error("Function %s expected %i arguments, we passed %i" % (
                    fn.name, len(fn.args), len(args),
                ))
            # Check all the named arguments.
            optional_args_values = {}
            for name, val in named_args.items():
                if name not in fn.named_args:
                    if not fn.named_args:
                        self.interpreter_error("Function %s doesn't take any optional arguments (%s was passed)" % (fn.name, name))
                    self.interpreter_error("Function %s doesn't have an optional argument called %s (%s's optional arguments: %r)" % (
                        fn.name, name, fn.name, list(fn.named_args),
                    ))
                self.type_check_assert(val, fn.named_args[name])
                optional_args_values[name] = val
            subscope = scope.copy()
            subscope["@purpose_name"] = purpose_name
            subscope["@optional_args"] = optional_args_values
            # Only update the name prefix if we're not immediately calling a builtin function.
            if fn.do_prefix_names:
                subscope["@name_prefix"] = prefix_join(scope["@name_prefix"], self.get_unique(fn_name))
            if isinstance(fn.args, int):
                subscope["@args"] = args
            else:
                for arg, (arg_name, arg_ty_annot) in zip(args, fn.args):
                    self.type_check_assert(arg, arg_ty_annot)
                    subscope[arg_name] = arg
            # Perform the function call.
            if isinstance(fn.body, list):
                return self.execute(subscope, fn.body)
            else:
                return fn.body(self, subscope)
        elif kind == "known-value":
            # This path is just used internally by the compiler, and is not generated in any of our ASTs.
            return expr[1]
        self.interpreter_error("Bug! Unhandled expr: %r" % (expr,))

    def evaluate_unit_expr(self, unit_expr):
        kind = unit_expr[0]
        if kind == "var":
            return self.unit_system.parse_unit_name(unit_expr[1])
        elif kind == "binary-op":
            pass
            #if 
        self.interpreter_error("Invalid operation in unit expression: %r" % (unit_expr,))

    def register_variable(self, name):
        if name not in self.all_variables:
            self.all_variables[name] = StateVariable(name)
        if name.endswith("'"):
            self.register_variable(name[:-1])
        return self.all_variables[name]

    def make_realized_process(self, name, args):
        index = len(self.realized_processes)
        self.realized_processes.append((name, args))
        return index

    def obliterate_variable(self, base_name):
        order = 0
        while True:
            name = base_name + "'" * order
            if name not in self.all_variables:
                break
            self.all_variables.pop(name)
            if name in self.variable_drivers:
                self.variable_drivers.pop(name)
            if name in self.variable_initializers:
                self.variable_initializers.pop(name)
            order += 1

    def get_compilation_parameter(self, desired_name, default_value):
        # First, determine the final name that we're giving to this request.
        final_name = dict_insert_no_collision(self.compilation_parameters, desired_name, lambda final_name: final_name)
        # If we have an external definition for this value then get this value.
        return final_name, self.external_compilation_parameters.get(final_name, default_value)

    def make_adjustable_parameter(self, desired_name, default_value, name_must_be_exact=True):
        if desired_name not in self.adjustable_parameters:
            adj_param = AdjustableParameter(desired_name, default_value)
            self.adjustable_parameters[desired_name] = adj_param
            return adj_param
        elif name_must_be_exact:
            self.interpreter_error("Attempt to redefine parameter: %s" % desired_name)
        return dict_insert_no_collision(
            self.adjustable_parameters,
            desired_name,
            lambda final_name: AdjustableParameter(final_name, default_value),
        )

    def compile_time_assign(self, scope, name, value, reassign=False):
        if name in scope and not reassign:
            self.interpreter_error("Redefinition of %s" % (name,))
        scope[name] = value

    def lookup(self, scope, name):
        if name not in scope:
            self.interpreter_error("Undefined name: %s" % (name,))
        return scope[name]

    def set_initializer(self, lhs, rhs):
        if not lhs.IS_VAR:
            self.interpreter_error("LHS of ~ must be a var, not: %r" % (lhs,))
        if not rhs.LAYER >= LAYER_CONST:
            self.interpreter_error("RHS of ~ must be constant.")
        # We add a prime here because if a variable is initialized then it's dynamic, and thus has a derivative.
        self.register_variable(lhs.name + "'")
        if lhs.name in self.variable_initializers:
            # TODO: Get both line numbers.
            self.interpreter_error("Attempt to double-initialize %s" % lhs.name)
        self.variable_initializers[lhs.name] = rhs

    def set_driver(self, lhs, rhs):
        if not lhs.IS_VAR:
            self.interpreter_error("LHS of <- must be a var, not: %r" % (lhs,))
        self.register_variable(lhs.name)
        if lhs.name in self.variable_drivers:
            self.interpreter_error("Attempt to double-drive %s" % lhs.name)
        self.variable_drivers[lhs.name] = rhs

    def add_to_driver(self, lhs, rhs):
        if not lhs.IS_VAR:
            self.interpreter_error("LHS of <- must be a var, not: %r" % (lhs,))
        self.register_variable(lhs.name)
        if lhs.name in self.variable_drivers:
            self.variable_drivers[lhs.name] = Expr(
                layer=LAYER_DYN,
                op="+",
                args=[self.variable_drivers[lhs.name], rhs],
            )
        else:
            self.variable_drivers[lhs.name] = rhs

    #def constify_via_state(self, expr):
    #    name = "@random_param%i" % len(self.all_variables)
    #    var = self.register_variable(name)
    #    self.set_initializer(var, expr)
    #    self.set_driver(self.all_variables[name + "'"], CompileTimeData(float, 0.0))
    #    return var

    def constify_via_param(self, name, expr):
        param = self.make_adjustable_parameter(
            desired_name="@random_" + name,
            default_value=0.0,
            name_must_be_exact=False,
        )
        self.expr_initialized_parameters[param.name] = expr
        return param

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
            elif kind == "simple-plot":
                _, var_exprs = statement
                globalTime = self.evaluate_expr(scope, ("var", "globalTime"), "")
                for var_expr in var_exprs:
                    val = self.evaluate_expr(scope, var_expr, "plot")
                    desired_name = "plot"
                    # If we depend on just one variable then auto-name based on that.
                    if len(val.get_deps()) == 1:
                        desired_name = val.get_deps().pop().name
                    def make_plot_desc(final_name):
                        layout = copy.deepcopy(SIMPLE_PLOT_DEFAULT_LAYOUT)
                        layout["title"] = final_name
                        return {
                            "dataTemplates": [
                                {"xExpr": globalTime, "yExpr": val, "settings": copy.deepcopy(DEFAULT_PLOT_DATA_SETTINGS)},
                            ],
                            "layout": layout,
                        }
                    dict_insert_no_collision(self.plots, desired_name, make_plot_desc)
                    #dict_insert_no_collision(self.plots, desired_name, lambda final_name: {
                    #    "expr": val,
                    #    "layout": ,
                    #})
            elif kind == "complex-plot":
                _, body = statement
                subscope = scope.copy()
                new_plot_name = self.get_unique("complex_plot")
                layout = copy.deepcopy(SIMPLE_PLOT_DEFAULT_LAYOUT)
                layout["title"] = new_plot_name
                subscope["@current_plot"] = self.plots[new_plot_name] = {
                    "dataTemplates": [],
                    "layout": layout,
                    "envelopePeriod": None,
                }
                return_value = self.execute(subscope, body)
                if return_value is not None:
                    return return_value
            elif kind in ("title", "trace", "trace2d", "layout"):
                current_plot = scope["@current_plot"]
                if current_plot is None:
                    self.interpreter_error("Use of %s outside of plot." % kind)
                if kind == "title":
                    _, title_expr = statement
                    title = self.evaluate_expr(scope, title_expr, "title")
                    if title.LAYER != LAYER_COMPTIME and title.ty == str:
                        self.interpreter_error("Plot title must be a string")
                    current_plot["layout"]["title"] = title.value
                elif kind == "trace":
                    _, x_expr, y_expr, settings_overlay = statement
                    settings = copy.deepcopy(DEFAULT_PLOT_DATA_SETTINGS)
                    self.overlay_json(settings, settings_overlay)
                    trace_x = self.evaluate_expr(scope, x_expr, "trace")
                    trace_y = self.evaluate_expr(scope, y_expr, "trace")
                    if isinstance(trace_y, ArrayVariable):
                        self.interpreter_error("Cannot use trace on an array, instead try:\n  for $i in 0 .. len($array) {\n    trace globalTime, $array[$i] { ... }\n  }")
                    current_plot["dataTemplates"].append({
                        "xExpr": trace_x,
                        "yExpr": trace_y,
                        "settings": settings,
                    })
                elif kind == "trace2d":
                    _, x_expr, z_array_expr, y_pitch_expr, settings_overlay = statement
                    settings = copy.deepcopy(DEFAULT_HEATMAP_DATA_SETTINGS)
                    self.overlay_json(settings, settings_overlay)
                    trace_x     = self.evaluate_expr(scope, x_expr, "trace2d")
                    trace_y     = self.evaluate_expr(scope, z_array_expr, "trace2d")
                    trace_pitch = self.evaluate_expr(scope, y_pitch_expr, "pitch")
                    if not isinstance(trace_y, ArrayVariable):
                        self.interpreter_error("Can only use trace2d on an array")
                    if trace_pitch.LAYER != LAYER_COMPTIME or trace_pitch.ty not in (int, float):
                        self.interpreter_error("The second argument to trace2d must be an expression for the y pitch")
                    settings["y"] = [trace_pitch.value * i for i in range(trace_y.length)]
                    current_plot["dataTemplates"].append({
                        "xExpr": trace_x,
                        "rowExprs": [
                            self.evaluate_expr(scope, ("binary-op", "[]", ("known-value", trace_y), ("lit", "int", i)), "trace2d")
                            for i in range(trace_y.length)
                        ],
                        "settings": settings,
                    })
                elif kind == "layout":
                    _, layout_overlay = statement
                    self.overlay_json(current_plot["layout"], layout_overlay)
                else:
                    self.interpreter_error("Bug: Unhandled plot case.")
            elif kind == "simoptions":
                _, new_options = statement
                for k, v in new_options.items():
                    self.settings[k] = v
            elif kind == "expr":
                _, e = statement
                expr_kind = e[0]
                if expr_kind == "binary-op" and e[1] in {"~", "<-", "<-+-"}:
                    _, op_kind, lhs, rhs = e
                    lhs = self.evaluate_expr(scope, lhs, "param")
                    purpose_name = self.get_purpose_name_from(lhs) + {
                        "~": "_init", "<-": "_param", "<-+-": "_param"
                    }[op_kind]
                    rhs = self.evaluate_expr(scope, rhs, purpose_name=purpose_name)
                    if op_kind == "~":
                        self.set_initializer(lhs, rhs)
                    elif op_kind == "<-":
                        self.set_driver(lhs, rhs)
                    elif op_kind == "<-+-":
                        self.add_to_driver(lhs, rhs)
                elif expr_kind == "binary-op" and e[1] == "=":
                    _, op_kind, lhs, rhs = e
                    if lhs[0] != "compvar":
                        self.interpreter_error("Assignment must have $variable as LHS")
                    _, var_name = lhs
                    self.compile_time_assign(scope, var_name, self.evaluate_expr(scope, rhs, var_name), reassign=True)
                elif expr_kind == "call":
                    result = self.evaluate_expr(scope, e, "param")
                else:
                    self.interpreter_error("Invalid expression. Only (x ~ y), (x <- y), (x <-+- y), (x := y), (x = y), and function calls allowed at top-level.")
            elif kind == "let":
                _, let_desc = statement
                value = self.evaluate_expr(scope, let_desc["initializer"], let_desc["name"])
                if let_desc["type"] is not None:
                    self.type_check_assert(value, let_desc["type"])
                self.compile_time_assign(scope, let_desc["name"], value, reassign=False)
            elif kind == "array":
                _, var_base_name_expr, array_length_expr = statement
                # Get the array name.
                var_base = self.evaluate_expr(scope, var_base_name_expr, "array_name")
                if not var_base.IS_VAR:
                    self.interpreter_error("array declaration must be like: array var[length];")
                if var_base.name.endswith("'"):
                    self.interpreter_error("array variable should have no primes (like array var[3] instead of array var'[3])")
                # Get the array length.
                array_length = self.evaluate_expr(scope, array_length_expr, "array_len")
                if array_length.LAYER != LAYER_COMPTIME:
                    self.interpreter_error("array length must be a compile-time value")
                if array_length.ty != int:
                    self.interpreter_error("array length must be an integer")
                if array_length.value < 0:
                    self.interpreter_error("array length must be non-negative")
                # TODO: Properly handle conflicting non-array variables.
                # For now, just erase all existing knowledge of this variable.
                self.obliterate_variable(var_base.name)
                if var_base.name in self.array_variables:
                    self.interpreter_error("Redeclaration of array: %s" % var_base.name)
                self.array_variables[var_base.name] = ArrayVariable(var_base.name, array_length.value)
            elif kind == "for":
                _, for_desc = statement
                iterator = self.evaluate_expr(scope, for_desc["iterator"], "for_loop")
                if iterator.ty != tuple or len(iterator.value) != 2:
                    self.interpreter_error("Can only iterate over ranges like: (0 .. 10)")
                for iterator_value in range(iterator.value[0], iterator.value[1]):
                    self.compile_time_assign(scope, for_desc["name"], CompileTimeData(int, iterator_value))
                    return_value = self.execute(scope, for_desc["body"])
                    if return_value is not None:
                        return return_value
                    # Remove the binding of the loop variable.
                    scope.pop(for_desc["name"])
            elif kind == "if":
                _, if_desc = statement
                for (cond_expr, body) in if_desc["elifs"]:
                    cond = self.evaluate_expr(scope, cond_expr, "if")
                    if cond.LAYER != LAYER_COMPTIME:
                        self.interpreter_error("Condition for if must be compile-time")
                    if cond.value:
                        return_value = self.execute(scope, body)
                        if return_value:
                            return return_value
                        break
                else:
                    if if_desc["else"] is not None:
                        return_value = self.execute(scope, if_desc["else"])
                        if return_value:
                            return return_value
            elif kind == "return":
                _, e = statement
                if e is None:
                    return
                return self.evaluate_expr(scope, e, "param")
            elif kind == "envelopeperiod":
                _, e = statement
                period = self.evaluate_expr(scope, e, "envelope_period")
                if period.LAYER != LAYER_COMPTIME:
                    self.interpreter_error("Envelope period must be a compile-time value")
                scope["@current_plot"]["envelopePeriod"] = period.value
            elif kind == "new_unit":
                _, unit_desc = statement
                # Split up into the three cases: prefix, base unit, non-base unit.
                if unit_desc["options"]["prefix"]:
                    if unit_desc["definition"] is None:
                        self.interpreter_error("Prefix unit with no definition makes no sense")
                    units = self.evaluate_unit_expr(unit_desc["definition"])
                    if not units.is_dimensionless():
                        self.interpreter_error("Prefix unit must be dimensionless")
                    self.unit_system.add_prefix(unit_desc["name"], units.scalar)
                elif unit_desc["definition"] is None:
                    self.unit_system.add_base_unit(unit_desc["name"])
                else:
                    units = self.evaluate_unit_expr(unit_desc["definition"])
                    self.unit_system.add_defined_units(unit_desc["name"], units)
            elif kind == "optim":
                _, optim_desc = statement
                objective_var = self.evaluate_expr(scope, ("var", "_optimizationObjective"), "")
                self.set_driver(objective_var, self.evaluate_expr(scope, optim_desc["objective"], "objective"))
                tunables = []
                for tunable_expr in optim_desc["tunable"]:
                    t = self.evaluate_expr(scope, tunable_expr, "tunable")
                    # We demand that t be a param.
                    if not isinstance(t, AdjustableParameter):
                        self.interpreter_error("Tunable must be an expression that evaluates to a parameter")
                    tunables.append(t.name)
                self.widgets.append({
                    "kind": "optimizer",
                    "tunable": tunables,
                    "options": optim_desc["options"],
                })
            elif kind == "javascript":
                self.interpreter_error("Inline javascript not currently fully implemented")
            else:
                self.interpreter_error("Bug! Unhandled statement: %r" % (statement,))

    def codegen_shared(self):
        # Patch up our settings.
        if self.settings["minstep"] is None:
            self.settings["minstep"] = self.settings["stepsize"] * 0.01
        if self.settings["maxstep"] is None:
            self.settings["maxstep"] = self.settings["stepsize"] * 100

        self.codegen_variable_info = {}
        # Start by sanity-checking everything.
        variable_orders = collections.defaultdict(int)
        for var in self.all_variables:
            base_name, ticks = split_variable_name(var)
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
                self.global_interpreter_error("Variable %s isn't driven." % top_derivative)
            self.codegen_variable_info[base_name]["driver"] = self.variable_drivers[top_derivative]
            self.codegen_variable_info[base_name]["initializers"] = []
            for i in range(order):
                lower_derivative = base_name + "'" * i
                if lower_derivative in self.variable_drivers:
                    self.global_interpreter_error("Variable %s is driven, when %s should be." % (
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
                self.global_interpreter_error("Cyclic dependencies in zeroth order variables involving: %s" % node)
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
            self.plot_data_initial[plot_name] = [
                {"x": [], "z": [[]] * len(template["rowExprs"])} # This is the case for a trace2d.
                    if "rowExprs" in template else
                {"x": [], "y": []} # This is the case for a normal trace.
                for template in plot_spec["dataTemplates"]
            ]
            self.plots_desc[plot_name] = {
                "dataTemplates": [
                    template["settings"]
                    for template in plot_spec["dataTemplates"]
                ],
                "layout": plot_spec["layout"],
                "envelopePeriod": plot_spec["envelopePeriod"],
            }

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
            if expr.op == "WienerDerivativeUnstable":
                return "wienerDerivativeUnstable(dt)"
            if expr.op == "RealizedProcess":
                return "realizedProcessFunctions[%i](t)" % expr.realized_index
                #expr.
            fn_table = {
                "Uniform": "uniformRandom",
                "Gaussian": "gaussianRandom",
                "Gamma": "gammaRandom",
                "Beta": "betaRandom",
                "Frechet": "frechetRandom",
                "^": "Math.pow",
                "exp": "Math.exp",
                "log": "Math.log",
                "sin": "Math.sin",
                "cos": "Math.cos",
                "sqrt": "Math.sqrt",
                "abs": "Math.abs",
                "floor": "Math.floor",
                "ceil": "Math.ceil",
                "round": "Math.round",
                "min": "Math.min",
                "max": "Math.max",
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
            if isinstance(expr.value, bool):
                return {False: "false", True: "true"}[expr.value]
            return "%r" % (expr.value,)
        self.interpreter_error("Bug: Unhandled expr in JS codegen: %r" % (expr,))

    def codegen_numba_expr(self, expr):
        pass

    def codegen_js(self):
        self.codegen_shared()

        allocate_code = []
        for param_name, param in self.adjustable_parameters.items():
            allocate_code.append("parameters[%s /*%s*/] = %r;" % (
                self.parameter_allocation[param_name],
                param_name,
                self.codegen_js_expr(CompileTimeData(
                    type(param.default_value), param.default_value
                )),
            ))
        for process_name, _ in self.realized_processes:
            allocate_code.append("realizedProcesses.push(new Float64Array(realizedProcessLength)); // %s" % process_name)
        allocate_code = "\n".join(allocate_code)

        init_code = []
        init_code.append("// Expression initialized params")
        for param_name, expr in self.expr_initialized_parameters.items():
            init_code.append("parameters[%s /*%s*/] = %s;" % (
                self.parameter_allocation[param_name], param_name, self.codegen_js_expr(expr),
            ))
        init_code.append("// State initializers")
        for base_name in sorted(self.codegen_variable_info.keys()):
            info = self.codegen_variable_info[base_name]
            init_code.append("// " + base_name)
            for i, initializer in enumerate(info["initializers"]):
                v = base_name + "'" * i
                init_code.append("state[%s /*%s*/] = %s;" % (
                    self.slot_allocation[v], v, self.codegen_js_expr(initializer),
                ))
        init_code.append("// Realized processes")
        for process_index, (process_name, args_exprs) in enumerate(self.realized_processes):
            init_code.append("realizedProcessFunctions[%i] = make%s(realizedProcesses[%i], %s);" % (
                process_index, process_name, process_index,
                ", ".join(self.codegen_js_expr(arg) for arg in args_exprs),
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
            for i, template in enumerate(plot_spec["dataTemplates"]):
                extract_plot_datum.append("plotData[%s][%i].x.push(%s);" % (
                    repr(plot_name), i, self.codegen_js_expr(template["xExpr"]),
                ))
                # Check which kind of template this is.
                if "rowExprs" in template:
                    # Special trace2d case.
                    extract_plot_datum.append("fillRow = plotData[%s][%i].z;" % (repr(plot_name), i))
                    for row_index, row_expr in enumerate(template["rowExprs"]):
                        extract_plot_datum.append("fillRow[%i].push(%s);" % (
                            row_index, self.codegen_js_expr(row_expr)
                        ))
                else:
                    # Regular trace case.
                    extract_plot_datum.append("plotData[%s][%i].y.push(%s);" % (
                        repr(plot_name), i, self.codegen_js_expr(template["yExpr"]),
                    ))
        extract_plot_datum = "\n".join(extract_plot_datum)

        # This value of 42 should never be hit if it matters.
        objective_code = "42"
        if "_optimizationObjective" in self.all_variables:
            objective_code = self.codegen_js_expr(
                self.evaluate_expr(self.root_scope, ("var", "_optimizationObjective"), "")
            )

        subst = {
            "degrees_of_freedom": self.degrees_of_freedom,
            "scratch_buffer_size": self.scratch_buffer_size,
            "parameter_count": len(self.adjustable_parameters),
            "allocate_code": indent_all_lines(12, allocate_code).strip(),
            "initialization_code": indent_all_lines(12, init_code).strip(),
            "derivative_code": indent_all_lines(12, derivative_code).strip(),
            "extract_plot_datum": indent_all_lines(12, extract_plot_datum).strip(),
            "plot_data_initial": indent_all_lines(12, json.dumps(self.plot_data_initial, indent=4)).strip(),
            "objective_code": objective_code,
            "plots": indent_all_lines(8, json.dumps(self.plots_desc, indent=4)).strip(),
            "widgets": indent_all_lines(8, json.dumps(self.widgets, indent=4)).strip(),
            "parameter_table": json.dumps(self.parameter_allocation),
            "scratch_table": json.dumps(self.scratch_allocation),
            "state_table": json.dumps(self.slot_allocation),
            "settings": json.dumps(self.settings),
            # Settings that we expose for makeWienerProcess.
            "simtime": repr(self.settings["simtime"]),
            "processscale": repr(self.settings["processscale"]),
        }
        return """
(() => {
    let xoshiro128ss_state = [[1, 2, 3, 4]];
    const RANDOM_EPSILON = 0.5 / 4294967296;
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
            const u = RANDOM_EPSILON + myRandom(), v = myRandom();
            const scale = Math.sqrt(-2 * Math.log(u));
            const arg = 2 * Math.PI * v;
            boxMullerCache[0] = scale * Math.cos(arg);
            return scale * Math.sin(arg);
        }
        const val = boxMullerCache[0];
        boxMullerCache[0] = null;
        return val;
    }
    function gammaRandom(alpha, beta) {
        // Translated from the Python source code for random.py.
        if (alpha > 1.0) {
            // Uses R.C.H. Cheng, "The generation of Gamma
            // variables with non-integral shape parameters",
            // Applied Statistics, (1977), 26, No. 1, p71-74
            const ainv = Math.sqrt(2 * alpha - 1);
            const bbb = alpha - Math.log(4);
            const ccc = alpha + ainv;
            const SG_MAGICCONST = 1 + Math.log(4.5);
            while (true) {
                const u1 = myRandom()
                if (u1 < 1e-7 || u1 > 0.9999999)
                    continue;
                const u2 = 1 - myRandom();
                const v = Math.log(u1 / (1 - u1)) / ainv;
                const x = alpha * Math.exp(v);
                const z = u1 * u1 * u2;
                const r = bbb + ccc * v - x;
                if (r + SG_MAGICCONST - 4.5 * z >= 0.0 || r >= Math.log(z))
                    return x * beta;
            }
        } else if (alpha === 1.0) {
            return -Math.log(RANDOM_EPSILON + myRandom()) * beta;
        } else {
            // Uses ALGORITHM GS of Statistical Computing - Kennedy & Gentle
            let x;
            while (true) {
                const u = myRandom();
                const b = (Math.E + alpha) / Math.E;
                const p = b * u;
                if (p <= 1.0) {
                    x = Math.pow(p, 1 / alpha);
                } else {
                    x = -Math.log((b - p) / alpha);
                }
                const u1 = myRandom();
                if (p > 1.0) {
                    if (u1 <= Math.pow(x, alpha - 1))
                        break;
                } else if (u1 <= Math.exp(x)) {
                    break;
                }
            }
            return x * beta;
        }
    }
    function betaRandom(alpha, beta) {
        // Translated from the Python source code for random.py.
        const y = gammaRandom(alpha, 1.0);
        if (y === 0.0)
            return 0.0;
        return y / (y + gammaRandom(beta, 1.0));
    }
    function frechetRandom(alpha) {
        // From manually inverting the CDF for the Frechet distribution.
        // I think I did this right?
        return Math.pow(-Math.log(RANDOM_EPSILON + myRandom()), -1 / alpha);
    }
    function exponentialRandom() {
        return -Math.log(RANDOM_EPSILON + myRandom());
    }
    function wienerDerivativeUnstable(dt) {
        return gaussianRandom() / Math.sqrt(dt);
    }
    const realizedProcessLength = 1 + Math.ceil(%(simtime)s / %(processscale)s);
    function simpleLerp(realization) {
        return (t) => {
            const moment = Math.max(0, Math.min(realizedProcessLength - 2, t / %(processscale)s));
            const i = Math.floor(moment);
            // Numerical stability isn't a huge concern here. lerpCoef always being 0 is safe.
            const lerpCoef = moment - i;
            return realization[i] * (1 - lerpCoef) + realization[i + 1] * lerpCoef;
        };
    }
    function makePoissonProcess(realization, rate) {
        let v = 0.0;
        if (rate <= 0.0)
            throw "PoissonProcess rate ended up negative!";
        let nextStepTime = exponentialRandom() / rate;
        for (let i = 0; i < realizedProcessLength; i++) {
            realization[i] = v;
            nextStepTime -= %(processscale)s;
            while (nextStepTime <= 0.0) {
                nextStepTime += exponentialRandom() / rate
                v += 1.0;
            }
        }
        return simpleLerp(realization);
    }
    function makeWienerProcess(realization) {
        const scaling = Math.sqrt(%(processscale)s);
        let v = 0.0;
        for (let i = 0; i < realizedProcessLength; i++) {
            realization[i] = v;
            v += gaussianRandom() * scaling;
        }
        return simpleLerp(realization);
    }
    function makeWienerDerivative(realization) {
        const scaling = Math.sqrt(%(processscale)s);
        // Fill in the Wiener process that we're integrating up.
        for (let i = 0; i < realizedProcessLength; i++) {
            realization[i] = gaussianRandom() * scaling;
        }
        return (t) => {
            const moment = Math.max(0, Math.min(realizedProcessLength - 2, t / %(processscale)s));
            const i = Math.floor(moment);
            const lerpCoef = moment - i;
            return realization[i] / %(processscale)s;
            // Naively we could just return: realization[i] / %(processscale)s
            // But the discontinuity thereof makes our adaptive integration rule go crazy.
            // So instead we return a triangular hat that has the same integral, and thus twice the peak value.
            // Here the coefficient is 4 because we need a factor of 2 to make our peak twice as high,
            // and then another factor of 2 because Math.min(lerpCoef, 1 - lerpCoef) peaks at 0.5.
            const c = 4 * realization[i] / %(processscale)s;
            return c * Math.min(lerpCoef, 1 - lerpCoef);
        };
    }
    return {
        allocate: () => {
            const state = new Float64Array(%(degrees_of_freedom)s);
            const statePrime = new Float64Array(%(degrees_of_freedom)s);
            const scratch = new Float64Array(%(scratch_buffer_size)s);
            const parameters = new Float64Array(%(parameter_count)s);
            const plotData = {};
            const realizedProcesses = [];
            const realizedProcessFunctions = [];
            %(allocate_code)s
            return {state, statePrime, scratch, parameters, plotData, realizedProcesses, realizedProcessFunctions};
        },
        initialize: (ctx) => {
            ctx.plotData = %(plot_data_initial)s;
            const {state, scratch, parameters, realizedProcesses, realizedProcessFunctions} = ctx;
            // Ugh, this is ugly. This is to deal with how crappily I map seed -> state in the front-end.
            for (let i = 0; i < 100; i++)
                myRandom();
            %(initialization_code)s
        },
        getDerivative: (ctx, t, dt, state, statePrime) => {
            // NB: We don't necessarily use the statePrime from ctx!
            const {scratch, parameters, realizedProcessFunctions} = ctx;
            %(derivative_code)s
        },
        extractPlotDatum: (ctx, t, dt) => {
            const {state, scratch, parameters, plotData, realizedProcessFunctions} = ctx;
            let fillRow = null;
            %(extract_plot_datum)s
        },
        getObjective: (ctx, t, dt, state) => {
            const {scratch, parameters, realizedProcessFunctions} = ctx;
            return %(objective_code)s;
        },
        reseedRNG,
        getRNGState: () => [...xoshiro128ss_state[0]],
        setRNGState: (state) => {
            xoshiro128ss_state[0] = [...state];
            boxMullerCache[0] = null;
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
