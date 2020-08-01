"""
ODE DSL
"""

import dsl_parser

class InterpreterError(dsl_parser.SourcePositionError):
    NAME = "Interpreter"

class StateVariable:
    IS_VAR = True
    IS_CONST = False

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

class AdjustableParameter:
    IS_VAR = False
    IS_CONST = True

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "param<%s>" % self.name

class Expr:
    IS_VAR = False

    def __init__(self, is_const, op, args):
        self.IS_CONST = is_const
        self.op = op
        self.args = args

    def __repr__(self):
        return "%s(%s)" % (self.op, ", ".join(map(repr, self.args)))

class CompileTimeData:
    IS_VAR = False
    IS_CONST = True

    def __init__(self, ty, value):
        assert isinstance(value, ty)
        self.ty = ty
        self.value = value

    def __repr__(self):
        return repr(self.value)

class Function:
    def __init__(self, name, args, return_type, body):
        self.name = name
        self.args = args
        self.return_type = return_type
        self.body = body

def func_print(ctx, scope):
    print("Print called:", scope["val"])

def func_Slider(ctx, scope):
    pass

def func_Uniform(ctx, scope):
    pass

class Context:
    def __init__(self):
        self.all_variables = set()
        self.adjustable_parameters = {}
        self.variable_initializers = {}
        self.variable_drivers = {}
        self.root_scope = {
            "print": Function("print", [("val", None)], ("tuple", []), func_print),
            "Slider": Function("Slider", [("low", "float"), ("high", "float")], "const", func_Slider),
            "Uniform": Function("Uniform", [("low", "const"), ("high", "const")], "const", func_Uniform),
        }

        self.settings = {
            "tolerance": 1e-2,
            "simtime": None,
            "plot": set(),
        }
        self.most_recent_line_number = 1

    def interpreter_error(self, message):
        return InterpreterError(
            message,
            dsl_parser.StreamPos(line_number=self.most_recent_line_number, column_number=1),
        )

    def type_check_assert(self, value, ty):
        if ty is None:
            return
        # XXX: FIXME: TODO: Fill this in!

    def evaluate_expr(self, scope, expr):
        kind = expr[0]
        if kind == "var":
            _, var_name = expr
            self.register_variable(var_name)
            return StateVariable(var_name)
        elif kind == "compvar":
            return self.lookup(expr[1])
        elif kind == "lit":
            _, ty_name, value = expr
            return CompileTimeData({"int": int, "float": float, "str": str}[ty_name], value)
        elif kind == "call":
            _, fn_expr, arg_exprs = expr
            if fn_expr[0] != "var":
                raise self.interpreter_error("For now only simple function calls are allowed")
            _, fn_name = fn_expr
            fn = self.lookup(scope, fn_name)
            args = [self.evaluate_expr(scope, arg_expr) for arg_expr in arg_exprs]
            # Make sure that our arguments line up.
            if len(fn.args) != len(args):
                raise self.interpreter_error("Function %s expected %i arguments, we passed %i" % (
                    fn.name, len(fn.args), len(args),
                ))
            subscope = scope.copy()
            for arg, (arg_name, arg_ty_annot) in zip(args, fn.args):
                self.type_check_assert(arg, arg_ty_annot)
                subscope[arg_name] = arg
            if isinstance(fn.body, list):
                return self.execute(subscope, fn.body)
            else:
                return fn.body(self, subscope)
        raise self.interpreter_error("Bug! Unhandled expr: %r" % (expr,))

    def register_variable(self, name):
        self.all_variables.add(name)
        if name.endswith("'"):
            self.register_variable(name[:-1])

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
        if not rhs.IS_CONST:
            raise self.interpreter_error("RHS of ~ must be constant.")
        self.register_variable(lhs.name)
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

    def execute(self, scope, program):
        if scope is None:
            scope = self.root_scope
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
                self.settings["plot"].update(self.evaluate_expr(scope, var_expr) for var_expr in var_exprs)
            elif kind in {"tolerance", "simtime"}:
                _, value = statement
                self.settings[kind] = value
            elif kind == "expr":
                _, e = statement
                expr_kind = e[0]
                if expr_kind == "binary-op" and e[1] in {"~", "<-"}:
                    _, op_kind, lhs, rhs = e
                    lhs = self.evaluate_expr(scope, lhs)
                    rhs = self.evaluate_expr(scope, rhs)
                    if op_kind == "~":
                        self.set_initializer(lhs, rhs)
                    elif op_kind == "<-":
                        self.set_driver(lhs, rhs)
                else:
                    raise self.interpreter_error("Invalid expression. Only (x ~ y) and (x <- y) allowed at top-level.")
            else:
                raise self.interpreter_error("Bug! Unhandled statement: %r" % (statement,))

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

freq ~ Slider(1, 2);
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
        print(var, "~", ctx.variable_drivers.get(var, "(0)"))
