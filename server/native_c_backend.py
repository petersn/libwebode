
import os
import time
import math
import random
import ctypes
import tempfile
import subprocess
import numpy as np
import dsl
import native_backend

PARALLEL = True

with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "native_template.c")) as f:
    C_TEMPLATE = f.read()

#TEMP_DIR = tempfile.mkdtemp()

def compile_c_module(c_source):
    #print(c_source)
    #with open("/tmp/mod.c", "w") as f:
    #    f.write(c_source)
    _, so_path = tempfile.mkstemp(suffix=".so") #, dir=TEMP_DIR)
    #print(so_path)
    gcc_command = ["gcc", "-Ofast", "-std=c99", "-shared", "-fPIC", "-x", "c", "-", "-o", so_path]
    if PARALLEL:
        gcc_command[1:1] = ["-fopenmp"]
    stdout, stderr = subprocess.Popen(
        gcc_command,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).communicate(c_source.encode())
    print("======================")
    print("=== C compiler stdout:")
    print(stdout.decode())
    print("=== C compiler stderr:")
    print(stderr.decode())
    print("======================")
    cdll = ctypes.CDLL(so_path)
    os.unlink(so_path)
    return cdll

class CBackend(native_backend.NativeBackend):
    BACKEND_NAME = "native-c"

    def __init__(self, ctx):
        super().__init__(ctx)
        self.module = compile_c_module(self.make_c_module_source())

        self.module.set_rng_state.argtypes = [ctypes.c_int] * 4
        self.module.set_rng_state.restype = None

        self.module.initialize.argtypes = [ctypes.c_uint64] * 3
        self.module.initialize.restype = None

        self.module.run_simulation.argtypes = [ctypes.c_uint64] * 8
        self.module.run_simulation.restype = ctypes.c_int

    def make_c_module_source(self):
        init_code = []
        init_code.append("// Expression initialized params")
        for param_name, expr in self.ctx.expr_initialized_parameters.items():
            init_code.append("parameters[%s] = %s;" % (
                self.ctx.parameter_allocation[param_name], self.ctx.codegen_c_expr(expr),
            ))
        init_code.append("// State initializers")
        for base_name in sorted(self.ctx.codegen_variable_info.keys()):
            info = self.ctx.codegen_variable_info[base_name]
            init_code.append("// " + base_name)
            for i, initializer in enumerate(info["initializers"]):
                v = base_name + "'" * i
                init_code.append("state[%s] = %s;" % (
                    self.ctx.slot_allocation[v], self.ctx.codegen_c_expr(initializer),
                ))
        init_code.append("// Realized processes")
        for process_index, (process_name, args_exprs) in enumerate(self.ctx.realized_processes):
            init_code.append("make_%s(realized_processes + %i * REALIZED_PROCESS_LENGTH%s);" % (
                process_name, process_index,
                "".join(", " + self.ctx.codegen_c_expr(arg) for arg in args_exprs),
            ))
        init_code = dsl.indent_all_lines(8, "\n".join(init_code)).strip()

        # Simulation code.
        step_size = self.ctx.settings["stepsize"]
        simtime = self.ctx.settings["simtime"]
        # Ugh, this 1e-6 is so ugly.
        plot_cooldown_increment = 1e-6 + self.total_steps / self.plot_trace_length
        desired_final_plot_length = self.plot_trace_length
        subst = {
            "realized_process_length": repr(self.realized_process_length),
            "init_code": init_code,

            "mcsamples": repr(self.mcsamples),
            "total_steps": repr(self.total_steps),
            "step_size": repr(step_size),
            "degrees_of_freedom": repr(self.ctx.degrees_of_freedom),
            "plot_cooldown_increment": repr(plot_cooldown_increment),
            "desired_final_plot_length": repr(desired_final_plot_length),
            "scratch_buffer_size": repr(self.ctx.scratch_buffer_size),
            "adjustable_parameters": repr(len(self.ctx.adjustable_parameters)),
            "realized_processes": repr(len(self.ctx.realized_processes)),
            "process_scale": self.ctx.settings["processscale"],
            "plot_trace_count": self.plot_trace_count,
            "plot_trace_length": self.plot_trace_length,
        }

        # Compile our derivative code.
        for derivative_index in range(4):
            derivative_code = ["// === Compute zeroth order ==="]
            for zo in self.ctx.zo_topo_sorted:
                derivative_code.append("scratch[%s] = %s;" % (
                    self.ctx.scratch_allocation[zo],
                    self.ctx.codegen_c_expr(self.ctx.codegen_variable_info[zo]["driver"]),
                ))
            derivative_code.append("// === Compute derivatives ===")
            for base_name in sorted(self.ctx.codegen_variable_info.keys()):
                info = self.ctx.codegen_variable_info[base_name]
                if info["order"] == 0:
                    continue
                derivative_code.append("// " + base_name)
                for i in range(info["order"] - 1):
                    v = base_name + "'" * i
                    derivative_code.append("state_prime%i[%s] = state[%s]; // Implicit" % (
                        derivative_index, self.ctx.slot_allocation[v], self.ctx.slot_allocation[v + "'"],
                    ))
                top_state = base_name + "'" * (info["order"] - 1)
                derivative_code.append("state_prime%i[%s] = %s;" % (
                    derivative_index, self.ctx.slot_allocation[top_state], self.ctx.codegen_c_expr(info["driver"]),
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
                extract_plot_datum.append("plotting_traces[%i * PLOT_TRACE_LENGTH + plot_counter] = %s;" % (
                    alloc((plot_name, i, "x")), self.ctx.codegen_c_expr(dataTemplate["xExpr"]),
                ))
                # Check which kind of dataTemplate this is.
                if "rowExprs" in dataTemplate:
                    # Special trace2d case.
                    for row_index, row_expr in enumerate(dataTemplate["rowExprs"]):
                        extract_plot_datum.append("plotting_traces[%i * PLOT_TRACE_LENGTH + plot_counter] = %s;" % (
                            alloc((plot_name, i, "z", row_index)),
                            self.ctx.codegen_c_expr(row_expr),
                        ))
                else:
                    # Regular trace case.
                    extract_plot_datum.append("plotting_traces[%i * PLOT_TRACE_LENGTH + plot_counter] = %s;" % (
                        alloc((plot_name, i, "y")), self.ctx.codegen_c_expr(dataTemplate["yExpr"]),
                    ))
        subst["extract_plot_datum"] = dsl.indent_all_lines(16, "\n".join(extract_plot_datum)).strip()

        subst["objective_expression"] = "42"
        if "_optimizationObjective" in self.ctx.all_variables:
            subst["objective_expression"] = self.ctx.codegen_c_expr(
                self.ctx.evaluate_expr(self.ctx.root_scope, ("var", "_optimizationObjective"), "")
            )

        return C_TEMPLATE % subst

    def set_rng_state(self, quadruple):
        self.module.set_rng_state(*quadruple)

    def do_computation(self, request):
        if self.ctx.settings["integrator"] != "rk4":
            self.ctx.interpreter_error("Backend native-c only supports integrator=rk4")

        # Phase 1: Initialize.
        start = time.time()
        self.module.initialize(
            self.state.ctypes.data,
            self.parameters.ctypes.data,
            self.realized_processes.ctypes.data,
        )
        end = time.time()
        print("Initialization time:", end - start)

        # Phase 2: Integrate.
        start = time.time()
        plot_length = self.module.run_simulation(
            self.state.ctypes.data,
            self.stateBackup.ctypes.data,
            self.statePrime.ctypes.data,
            self.scratch.ctypes.data,
            self.parameters.ctypes.data,
            self.realized_processes.ctypes.data,
            self.plotting_traces.ctypes.data,
            self.objectives.ctypes.data,
        )
        end = time.time()
        print("Integration time:", end - start)

        return plot_length

if __name__ == "__main__":
    module = compile_c_module("""
int foo(int x, int y) {
    return x + 3 * y;
}
""")
    module.foo.argtypes = [ctypes.c_int, ctypes.c_int]
    module.foo.restype = ctypes.c_int
    print(module.foo(3, 4))
    module = compile_c_module("""
int foo(int x, int y) {
    return x + 5 * y;
}
""")
    module.foo.argtypes = [ctypes.c_int, ctypes.c_int]
    module.foo.restype = ctypes.c_int
    print(module.foo(3, 4))
