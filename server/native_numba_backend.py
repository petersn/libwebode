
import time
import math
import random
import numpy as np
import numba
import dsl
import native_backend

PARALLEL = False

class NumbaBackend(native_backend.NativeBackend):
    BACKEND_NAME = "native-numba"

    def __init__(self, ctx):
        super().__init__(ctx)
        self.jitted_init_function = self.make_init_function()
        self.jitted_sim_function = self.make_sim_function()

    def make_init_function(self):
        template = """

def initialize(
    #A, B, C, D,
    a, b, c, d,
    state,
    parameters,
    realized_processes,
):
    #for samp in numba.prange(%(mcsamples)s):
    for samp in range(%(mcsamples)s):
        for i in range(%(realized_process_length)s):
            %(init_code)s
"""

        # The indentation of this string literal is important!
        xoshiro128ss_invocation = """
            t = b << 9;
            r = a * 5;
            r = (r << 7 | r >> 25) * 9;
            c ^= a; d ^= b;
            b ^= c; a ^= d; c ^= t;
            d = d << 11 | d >> 21;
            val = r / 4294967296.0;
        """

        init_code = []
        init_code.append("# Expression initialized params")
        for param_name, expr in self.ctx.expr_initialized_parameters.items():
            init_code.append("parameters[samp, %s] = %s;" % (
                self.ctx.parameter_allocation[param_name], self.ctx.codegen_numba_expr(expr),
            ))
        init_code.append("# State initializers")
        for base_name in sorted(self.ctx.codegen_variable_info.keys()):
            info = self.ctx.codegen_variable_info[base_name]
            init_code.append("# " + base_name)
            for i, initializer in enumerate(info["initializers"]):
                v = base_name + "'" * i
                init_code.append("state[samp, %s] = %s;" % (
                    self.ctx.slot_allocation[v], self.ctx.codegen_numba_expr(initializer),
                ))
        init_code.append("# Realized processes")
        for process_index, (process_name, args_exprs) in enumerate(self.ctx.realized_processes):
            init_code.append("realizedProcessFunctions[%i] = make%s(realizedProcesses[%i], %s);" % (
                process_index, process_name, process_index,
                ", ".join(self.ctx.codegen_numba_expr(arg) for arg in args_exprs),
            ))
        init_code = dsl.indent_all_lines(12, "\n".join(init_code)).strip()

        #realized_processes[samp, 0, i] = val

        subst = {
            "mcsamples": repr(self.mcsamples),
            "realized_process_length": repr(self.realized_process_length),
            "init_code": init_code,
        }
        python_code = template % subst
        return self.compile_python_source_jitted(
            python_code, "initialize",
            locals={
                **{var: numba.uint32 for var in "ABCDabcdtr"},
                "val": numba.float64,
            },
        )

    def make_sim_function(self):
        template = """
def run_simulation(
    state,
    stateBackup,
    statePrime,
    scratch,
    parameters,
    realized_processes,
    plotting_traces,
):
    dt = %(step_size)s

    for samp in numba.prange(%(mcsamples)s):
        t = 0.0
        plot_cooldown = 0.0
        plot_counter = 0
        for step in range(%(total_steps)s):
            # TODO: Benchmark these two.
            #stateBackup[samp, :] = state[samp, :]
            for i in range(%(degrees_of_freedom)s):
                stateBackup[samp, i] = state[samp, i]

            # Derivative at t = 0
            %(compute_deriv0)s

            # Derivative at t = 1/2
            for i in range(%(degrees_of_freedom)s):
                state[samp, i] = stateBackup[samp, i] + (0.5 * %(step_size)s) * statePrime[samp, 0, i]
            %(compute_deriv1)s

            # Derivative at t = 1/2
            for i in range(%(degrees_of_freedom)s):
                state[samp, i] = stateBackup[samp, i] + (0.5 * %(step_size)s) * statePrime[samp, 1, i]
            %(compute_deriv2)s

            # Derivative at t = 1
            for i in range(%(degrees_of_freedom)s):
                state[samp, i] = stateBackup[samp, i] + %(step_size)s * statePrime[samp, 2, i]
            %(compute_deriv3)s

            # Take a step.
            for i in range(%(degrees_of_freedom)s):
                state[samp, i] = stateBackup[samp, i] + (%(step_size)s / 6.0) * (
                            statePrime[samp, 0, i]
                    + 2.0 * statePrime[samp, 1, i]
                    + 2.0 * statePrime[samp, 2, i]
                    +       statePrime[samp, 3, i]
                )
            t += %(step_size)s

            # Possibly grab a data point for plotting.
            plot_cooldown -= 1.0
            if plot_cooldown <= 0.0:
                plot_cooldown += %(plot_cooldown_increment)s
                %(extract_plot_datum)s
                plot_counter += 1

    return plot_counter
"""
        step_size = self.ctx.settings["stepsize"]
        simtime = self.ctx.settings["simtime"]
        total_steps = math.ceil(simtime / step_size)
        # Ugh, this 1e-6 is so ugly.
        plot_cooldown_increment = 1e-6 + total_steps / self.plot_trace_length
        desired_final_plot_length = self.plot_trace_length
        subst = {
            "mcsamples": repr(self.mcsamples),
            "total_steps": repr(total_steps),
            "step_size": repr(step_size),
            "degrees_of_freedom": repr(self.ctx.degrees_of_freedom),
            "plot_cooldown_increment": repr(plot_cooldown_increment),
            "desired_final_plot_length": repr(desired_final_plot_length),
        }

        # Compile our derivative code.
        for derivative_index in range(4):
            derivative_code = ["# === Compute zeroth order ==="]
            for zo in self.ctx.zo_topo_sorted:
                derivative_code.append("scratch[samp, %s] = %s;" % (
                    self.ctx.scratch_allocation[zo],
                    self.ctx.codegen_numba_expr(self.ctx.codegen_variable_info[zo]["driver"]),
                ))
            derivative_code.append("# === Compute derivatives ===")
            for base_name in sorted(self.ctx.codegen_variable_info.keys()):
                info = self.ctx.codegen_variable_info[base_name]
                if info["order"] == 0:
                    continue
                derivative_code.append("# " + base_name)
                for i in range(info["order"] - 1):
                    v = base_name + "'" * i
                    derivative_code.append("statePrime[samp, %s] = state[samp, %s]; # Implicit" % (
                        self.ctx.slot_allocation[v], self.ctx.slot_allocation[v + "'"],
                    ))
                top_state = base_name + "'" * (info["order"] - 1)
                derivative_code.append("statePrime[samp, %s] = %s;" % (
                    self.ctx.slot_allocation[top_state], self.ctx.codegen_numba_expr(info["driver"]),
                ))

            subst["compute_deriv%i" % derivative_index] = \
                dsl.indent_all_lines(12, "\n".join(derivative_code)).strip()

        # Compile plot datum extraction.
        self.plotting_traces_allocation = {}
        def alloc(trace_specifier):
            index = len(self.plotting_traces_allocation)
            self.plotting_traces_allocation[trace_specifier] = index
            return index

        extract_plot_datum = []
        for plot_name, plot_spec in self.ctx.plots.items():
            for i, dataTemplate in enumerate(plot_spec["dataTemplates"]):
                extract_plot_datum.append("plotting_traces[samp, %i, plot_counter] = %s;" % (
                    alloc((plot_name, i, "x")), self.ctx.codegen_numba_expr(dataTemplate["xExpr"]),
                ))
                # Check which kind of dataTemplate this is.
                if "rowExprs" in dataTemplate:
                    # Special trace2d case.
                    for row_index, row_expr in enumerate(dataTemplate["rowExprs"]):
                        extract_plot_datum.append("plotting_traces[samp, %i, plot_counter] = %s;" % (
                            alloc((plot_name, i, "z", row_index)),
                            self.ctx.codegen_numba_expr(row_expr),
                        ))
                else:
                    # Regular trace case.
                    extract_plot_datum.append("plotting_traces[samp, %i, plot_counter] = %s;" % (
                        alloc((plot_name, i, "y")), self.ctx.codegen_numba_expr(dataTemplate["yExpr"]),
                    ))
        subst["extract_plot_datum"] = dsl.indent_all_lines(16, "\n".join(extract_plot_datum)).strip()

        python_code = template % subst
        return self.compile_python_source_jitted(python_code, "run_simulation")

    def compile_python_source_jitted(self, python_code, function_name, locals=None):
        print("=== Compiling ===")
        print(python_code)
        locals_dict = {}
        compiled = compile(python_code, "libwebode", "exec")
        exec(compiled, globals(), locals_dict)
        f = locals_dict[function_name]
        # JITify the function
        f = numba.njit(
            parallel=PARALLEL,
            fastmath=True,
            **{"locals": locals for _ in [1] if locals is not None},
        )(f)
        return f

    def set_rng_state(self, quadruple):
        # XXX: FIXME: Implement this!
        pass

    def do_computation(self, request):
        if self.ctx.settings["integrator"] != "rk4":
            self.ctx.interpreter_error("Backend native-numba only supports integrator=rk4")

        # Phase 1: Initialize.
        start = time.time()
        self.jitted_init_function(
            *[numba.uint32(random.getrandbits(32)) for _ in range(4)],
            state=self.state,
            parameters=self.parameters,
            realized_processes=self.realized_processes,
        )
        end = time.time()
        print("Initialization time:", end - start)

        # Phase 2: Integrate.
        start = time.time()
        plot_length = self.jitted_sim_function(
            state=self.state,
            stateBackup=self.stateBackup,
            statePrime=self.statePrime,
            scratch=self.scratch,
            parameters=self.parameters,
            realized_processes=self.realized_processes,
            plotting_traces=self.plotting_traces,
        )
        end = time.time()
        print("Integration time:", end - start)

        return plot_length
